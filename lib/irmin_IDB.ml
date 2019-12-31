(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt
open Iridb_utils
open Js_of_ocaml

let err_not_found n =
  Lwt.fail (Invalid_argument (Printf.sprintf "IndexedDB.%s: not found" n))

let db_name_key =
  Irmin.Private.Conf.(key "indexedDB.db_name" string "Irmin")

let ao = Iridb_lwt.store_name "ao_git"
let rw = Iridb_lwt.store_name "rw_git"

let ao_old = Iridb_lwt.store_name "ao"
let rw_old = Iridb_lwt.store_name "rw"

exception Format_too_old of [`Irmin_0_10]

let version = 4
let connect db_name =
  Iridb_lwt.make db_name ~version ~init:(fun ~old_version upgrader ->
    match old_version with
    | 0 ->
      Iridb_lwt.create_store upgrader ao;
      Iridb_lwt.create_store upgrader rw
    | 2 ->
      raise (Format_too_old `Irmin_0_10)
    | 3 ->
      (* Remove old stores from 2->3 migration. *)
      Iridb_lwt.delete_store upgrader ao_old;
      Iridb_lwt.delete_store upgrader rw_old
    | _ ->
      failwith "Attempt to upgrade from unknown schema version!"
  )

module RO (K: Irmin.Hum.S) (V: Tc.S0) = struct
  type t = Iridb_lwt.store

  let read t k =
    Iridb_lwt.get t (K.to_hum k) >|= function
    | None -> None
    | Some s -> Some (Tc.read_string (module V) s)

  let mem t k =
    Iridb_lwt.get t (K.to_hum k) >|= function
    | None -> false
    | Some _ -> true

  let iter t fn =
    Iridb_lwt.bindings t >>=
    Lwt_list.iter_p (fun (k, v) -> fn (K.of_hum k) (return (Tc.read_string (module V) v)))
end

module AO (K: Irmin.Hash.S) (V: Tc.S0) = struct
  include RO(K)(V)

  let create config =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    connect db_name >>= fun idb ->
    return (Iridb_lwt.store idb ao)

  let add t value =
    let k = Tc.write_cstruct (module V) value |> K.digest in
    let v = Tc.write_string (module V) value in
    Iridb_lwt.set t (K.to_hum k) v >|= fun () -> k
end

module RW (K: Irmin.Hum.S) (V: Irmin.Hash.S) = struct
  module W = Irmin.Private.Watch.Make(K)(V)

  type key = K.t
  type value = V.t
  type watch = W.watch

  type t = {
    r : Iridb_lwt.store;
    watch : W.t;
    prefix : string;
    notifications : Iridb_html_storage.t;
    mutable listener : (Dom.event_listener_id * int) option;
  }

  let create config =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    let prefix = db_name ^ ".rw." in
    let watch = W.create () in
    let notifications = Iridb_html_storage.make () in
    connect db_name >>= fun idb ->
    let r = Iridb_lwt.store idb rw in
    return { watch; r; prefix; notifications; listener = None }

  let string_of_hash x = Cstruct.to_string (V.to_raw x)
  let hash_of_string x = V.of_raw (Cstruct.of_string x)

  let read t k =
    Iridb_lwt.get t.r (K.to_hum k) >|= function
    | None -> None
    | Some s -> Some (hash_of_string s)

  let read_exn t key =
    read t key >>= function
    | Some v -> return v
    | None -> err_not_found "read"

  let mem t k =
    Iridb_lwt.get t.r (K.to_hum k) >|= function
    | None -> false
    | Some _ -> true

  let iter t fn =
    Iridb_lwt.bindings t.r >>=
    Lwt_list.iter_p (fun (k, v) -> fn (K.of_hum k) (return (hash_of_string v)))

  let ref_listener t =
    match t.listener with
    | None ->
        let l =
          Iridb_html_storage.watch t.notifications ~prefix:t.prefix (fun key value ->
            let subkey = tail key (String.length t.prefix) in
            let ir_key = K.of_hum subkey in
            let value = value >|?= hash_of_string in
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
    | Some v -> Iridb_html_storage.set t.notifications (t.prefix ^ K.to_hum k) (string_of_hash v)
    end;
    (* Notify this tab *)
    W.notify t.watch k new_value

  let update t k value =
    (* Log.warn "Non-atomic update called!"; *)
    string_of_hash value
    |> Iridb_lwt.set t.r (K.to_hum k) >>= fun () ->
    notify t k (Some value)

  let remove t k =
    (* Log.warn "Non-atomic remove called!"; *)
    Iridb_lwt.remove t.r (K.to_hum k) >>= fun () ->
    notify t k None

  let compare_and_set t k ~test ~set =
    let pred old =
      match old, test with
      | None, None -> true
      | Some old, Some expected -> hash_of_string old |> V.equal expected
      | _ -> false in
    let new_value = set >|?= string_of_hash in
    Iridb_lwt.compare_and_set t.r (K.to_hum k) ~test:pred ~new_value >>= function
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
end

let config db_name = Irmin.Private.Conf.singleton db_name_key db_name

(* From irmin-git *)
module Digest (H: Irmin.Hash.S): Git.SHA.DIGEST = struct
  (* FIXME: lots of allocations ... *)
  let cstruct buf = Git.SHA.of_raw (Cstruct.to_string (H.to_raw (H.digest buf)))
  let string str = cstruct (Cstruct.of_string str)
  let length = Cstruct.len @@ H.to_raw (H.digest (Cstruct.of_string ""))
end

module Make (C: Irmin.Contents.S) (T: Irmin.Ref.S) (H: Irmin.Hash.S) = struct
  (* We could just replace this with [Irmin.Make], but we want things in Git format so
     that we can sync with real Git repositories. *)

  module D = Digest(H)
  module Value_IO = Git.Value.IO(D)(Git.Inflate.None)

  module GitContents = struct
    (* Serialises Git objects (e.g. [Blob "foo"]) using the Git format (e.g. "blob 3\0foo").
       This means that they get the same hash that they would have got with the real Git. *)

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

    let create = Raw.create

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

  module Y = Irmin_git.Irmin_value_store(Git_store)(C)(H)
  include Irmin.Make_ext(struct
      module Contents = Irmin.Contents.Make(Y.Contents)
      module Node = Y.Node
      module Commit = Y.Commit
      module Ref = struct
        module Key = T
        module Val = H
        include RW (T)(H)
      end
      module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
      module Sync = Irmin.Private.Sync.None(Commit.Key)(T)
      module Repo = struct
        type t = {
          ao: Git_store.t;
          rw: Ref.t;
        }
        let ref_t t = t.rw
        let commit_t t = t.ao
        let node_t t = t.ao
        let contents_t t = t.ao

        let create config =
          Git_store.create config >>= fun ao ->
          Ref.create config       >>= fun rw ->
          Lwt.return { ao; rw }
      end
    end)
end
