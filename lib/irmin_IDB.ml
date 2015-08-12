(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt
open Iridb_utils

let err_not_found n =
  Lwt.fail (Invalid_argument (Printf.sprintf "IndexedDB.%s: not found" n))

let db_name_key =
  Irmin.Private.Conf.(key "indexedDB.db_name" string "Irmin")

let ao = Iridb_lwt.store_name "ao"
let rw = Iridb_lwt.store_name "rw"

let connect db_name =
  Iridb_lwt.make db_name ~init:(fun upgrader ->
    Iridb_lwt.create_store upgrader ao;
    Iridb_lwt.create_store upgrader rw;
  )

let version = 2

module RO (K: Irmin.Hum.S) (V: Tc.S0) = struct
  type key = K.t
  type value = V.t

  type t = {
    idb_store : Iridb_lwt.store;
    task : Irmin.task;
    config : Irmin.config;  (* Not used, but we need to provide it for some reason *)
  }

  let task t = t.task

  let make ~config idb_store task =
    return (fun a -> { task = task a; idb_store; config })

  let config t = t.config

  let read t k =
    Iridb_lwt.get t.idb_store (K.to_hum k) >|= function
    | None -> None
    | Some s -> Some (Tc.read_string (module V) s)

  let read_exn t key =
    read t key >>= function
    | Some v -> return v
    | None -> err_not_found "read"

  let mem t k =
    Iridb_lwt.get t.idb_store (K.to_hum k) >|= function
    | None -> false
    | Some _ -> true

  let iter { idb_store; _ } fn =
    Iridb_lwt.bindings idb_store >>=
    Lwt_list.iter_p (fun (k, v) -> fn (K.of_hum k) (return (Tc.read_string (module V) v)))
end

module AO (K: Irmin.Hash.S) (V: Tc.S0) = struct
  include RO(K)(V)

  let create config task =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    connect db_name ~version >>= fun idb ->
    make ~config (Iridb_lwt.store idb ao) task

  let add t value =
    let k = Tc.write_cstruct (module V) value |> K.digest in
    let v = Tc.write_string (module V) value in
    Iridb_lwt.set t.idb_store (K.to_hum k) v >|= fun () -> k
end

module RW (K: Irmin.Hum.S) (V: Tc.S0) = struct
  module W = Irmin.Private.Watch.Make(K)(V)
  module R = RO(K)(V)
  open R

  type watch = W.watch

  type t = {
    r : R.t;
    watch : W.t;
    prefix : string;
    notifications : Iridb_html_storage.t;
    mutable listener : (Dom.event_listener_id * int) option;
  }

  let create config task =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    let prefix = db_name ^ ".rw." in
    let watch = W.create () in
    let notifications = Iridb_html_storage.make () in
    connect db_name ~version >>= fun idb ->
    R.make ~config (Iridb_lwt.store idb rw) task >|= fun make_r ->
    fun task -> { watch; r = make_r task; prefix; notifications; listener = None }

  let config t = R.config t.r

  let ref_listener t =
    match t.listener with
    | None ->
        let l =
          Iridb_html_storage.watch t.notifications ~prefix:t.prefix (fun key value ->
            let subkey = tail key (String.length t.prefix) in
            let ir_key = K.of_hum subkey in
            let value = value >|?= Tc.read_string (module V) in
            async (fun () -> W.notify t.watch ir_key value)
          ) in
        t.listener <- Some (l, 1)
    | Some (l, n) ->
        t.listener <- Some (l, n + 1)

  let unref_listener t =
    match t.listener with
    | None -> failwith "unref_listener, but not listening!"
    | Some (l, 1) ->
        Dom.removeEventListener l;
        t.listener <- None
    | Some (l, n) ->
        assert (n > 1);
        t.listener <- Some (l, n - 1)

  let notify t k new_value =
    (* Notify other tabs *)
    begin match new_value with
    | None -> Iridb_html_storage.remove t.notifications (t.prefix ^ K.to_hum k)
    | Some v -> Iridb_html_storage.set t.notifications (t.prefix ^ K.to_hum k) (Tc.write_string (module V) v)
    end;
    (* Notify this tab *)
    W.notify t.watch k new_value

  let update t k value =
    (* Log.warn "Non-atomic update called!"; *)
    Tc.write_string (module V) value
    |> Iridb_lwt.set t.r.idb_store (K.to_hum k) >>= fun () ->
    notify t k (Some value)

  let remove t k =
    (* Log.warn "Non-atomic remove called!"; *)
    Iridb_lwt.remove t.r.idb_store (K.to_hum k) >>= fun () ->
    notify t k None

  let compare_and_set t k ~test ~set =
    let pred old =
      match old, test with
      | None, None -> true
      | Some old, Some expected -> Tc.read_string (module V) old |> V.equal expected
      | _ -> false in
    let new_value = set >|?= Tc.write_string (module V) in
    Iridb_lwt.compare_and_set t.r.idb_store (K.to_hum k) ~test:pred ~new_value >>= function
    | true -> notify t k set >|= fun () -> true
    | false -> return false

  let watch t ?init cb =
    ref_listener t;
    W.watch t.watch ?init cb

  let unwatch t w =
    unref_listener t;
    W.unwatch t.watch w

  let watch_key t key ?init cb =
    ref_listener t;
    W.watch_key t.watch key ?init cb

  let iter t = R.iter t.r
  let mem t = R.mem t.r
  let read t = R.read t.r
  let read_exn t = R.read_exn t.r
  let task t = R.task t.r
  type value = R.value
  type key = R.key
end

let config db_name = Irmin.Private.Conf.singleton db_name_key db_name

module Digest (H: Irmin.Hash.S): Git.SHA.DIGEST = struct
  (* FIXME: lots of allocations ... *)
  let cstruct buf = Git.SHA.of_raw (Cstruct.to_string (H.to_raw (H.digest buf)))
  let string str = cstruct (Cstruct.of_string str)
  let length = Cstruct.len @@ H.to_raw (H.digest (Cstruct.of_string ""))
end

module Make (C: Irmin.Contents.S) (T: Irmin.Tag.S) (H: Irmin.Hash.S) = struct
  (** We could just replace this with [Irmin.Make], but we want things in Git format so
   * that we can sync with real Git repositories. *)

  module D = Digest(H)
  module Value_IO = Git.Value.IO(D)(Git.Inflate.None)

  module GitContents = struct
    (** Serialises Git objects (e.g. [Blob "foo"]) using the Git format (e.g. "blob 3\0foo").
     * This means that they get the same hash that they would have got with the real Git. *)

    type t = Git.Value.t
    let equal = Git.Value.equal
    let hash = Git.Value.hash
    let compare = Git.Value.compare
    let read = Value_IO.input
    let to_string x =
      let b = Buffer.create 1024 in
      Value_IO.add b x;
      Buffer.contents b
    let write x cs =
      let x = to_string x in
      let l = String.length x in
      Cstruct.blit_from_string x 0 cs 0 l;
      Cstruct.shift cs l
    let size_of x = to_string x |> String.length
    let of_json _ = failwith "of_json"
    let to_json _ = failwith "to_json"
  end

  (* AO store for Git.Value.t objects *)
  module Git_store = struct
    (* Slight mismatch here, because we provide an Irmin API not a Git one *)
    module Raw = AO(H)(GitContents)

    type t = Raw.t

    let create config =
      Raw.create config Irmin.Task.none >|= fun x -> x ()

    let hash_of_digest d =
      Git.SHA.to_raw d |> Cstruct.of_string |> H.of_raw

    let digest_of_hash hash =
      H.to_raw hash |> Cstruct.to_string |> Git.SHA.of_raw

    let read t d =
      Raw.read t (hash_of_digest d)

    let mem t d =
      Raw.mem t (hash_of_digest d)

    let write t v =
      Raw.add t v >|= digest_of_hash

    let contents t =
      let results = ref [] in
      Raw.iter t (fun k v ->
        v >>= fun v ->
        results := (digest_of_hash k, v) :: !results;
        return ()
      ) >|= fun () ->
      !results

    module Digest = D
  end

  module X = Irmin_git.Irmin_value_store(Git_store)(C)(H)
  include Irmin.Make_ext(struct
    module Contents = Irmin.Contents.Make(X.Contents)
    module Node = X.Node
    module Commit = X.Commit
    module Tag = struct
      module Key = T
      module Val = H
      include RW (T)(H)
    end
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = Irmin.Private.Sync.None(Commit.Key)(T)
  end)
end
