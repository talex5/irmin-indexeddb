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

let version = 3
let connect db_name =
  Iridb_lwt.make db_name ~version ~init:(fun ~old_version upgrader ->
    match old_version with
    | 0 ->
      Iridb_lwt.create_store upgrader ao_old;
      Iridb_lwt.create_store upgrader rw_old;
      Iridb_lwt.create_store upgrader ao;
      Iridb_lwt.create_store upgrader rw;
    | 2 ->
      Iridb_lwt.create_store upgrader ao;
      Iridb_lwt.create_store upgrader rw;
    | _ ->
      failwith "Attempt to upgrade from unknown schema version!"
  )

module RO (K: Irmin.Hum.S) (V: Tc.S0) = struct
  type key = K.t
  type value = V.t

  type t = Iridb_lwt.store

  let read t k =
    Iridb_lwt.get t (K.to_hum k) >|= function
    | None -> None
    | Some s -> Some (Tc.read_string (module V) s)

  let read_exn t key =
    read t key >>= function
    | Some v -> return v
    | None -> err_not_found "read"

  let mem t k =
    Iridb_lwt.get t (K.to_hum k) >|= function
    | None -> false
    | Some _ -> true

  let iter t fn =
    Iridb_lwt.bindings t >>=
    Lwt_list.iter_p (fun (k, v) -> fn (K.of_hum k) (return (Tc.read_string (module V) v)))
end

module AO_old (K: Irmin.Hash.S) (V: Tc.S0) = struct
  include RO(K)(V)

  let create config =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    connect db_name >>= fun idb ->
    return (Iridb_lwt.store idb ao_old)

  let add _t _value = failwith "AO_old: read-only!"
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

module RW_old (K: Irmin.Hum.S) (V: Tc.S0) = struct
  module W = Irmin.Private.Watch.Make(K)(V)
  module R = RO(K)(V)

  type watch = W.watch

  type t = {
    r : R.t;
    watch : W.t;
  }

  let create config =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    let watch = W.create () in
    connect db_name >>= fun idb ->
    let r = Iridb_lwt.store idb rw_old in
    return { watch; r }

  let update _t _k _value = failwith "RW_old: update: read-only!"

  let remove _t _k = failwith "RW_old: remove: read-only!"

  let compare_and_set _t _k ~test:_ ~set:_ = failwith "RW_old: compare_and_set: read-only!"

  let watch t ?init cb =
    W.watch t.watch ?init cb

  let unwatch t w =
    W.unwatch t.watch w

  let watch_key t key ?init cb =
    W.watch_key t.watch key ?init cb

  let iter t = R.iter t.r
  let mem t = R.mem t.r
  let read t = R.read t.r
  let read_exn t = R.read_exn t.r
  type value = R.value
  type key = R.key
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

  module X = struct
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

  (* Don't know why we need to hide [slice] here - OCaml 4.05 compiler bug? *)
  include (X : module type of X with type slice := X.slice and module Repo := X.Repo)
  type slice = X.slice

  (* Migration from Irmin 0.10 format to Git format. *)
  module Old = Irmin.Make(AO_old)(RW_old)(C)(T)(H)
  module Topo = Graph.Topological.Make(Old.History)

  module Repo = struct
    include X.Repo

    let migrate_content t ~old c =
      Old.Private.Contents.read_exn (Old.Private.Repo.contents_t old) c >>=
      Private.Contents.add (Private.Repo.contents_t t)

    let rec migrate_tree t ~old tree =
      Old.Private.Node.read_exn (Old.Private.Repo.node_t old) tree >>= fun tree ->
      Old.Private.Node.Val.alist tree |> Lwt_list.map_s (function
          | s, `Contents c -> migrate_content t ~old c >|= fun c -> s, `Contents c
          | s, `Node dir -> migrate_tree t ~old dir >|= fun dir -> s, `Node dir
        ) >>= fun alist ->
      let tree = Private.Node.Val.create alist in
      Private.Node.add (Private.Repo.node_t t) tree

    let lwt_opt_map f = function
      | None -> Lwt.return_none
      | Some x -> f x >|= fun y -> Some y

    let create config =
      create config >>= fun t ->
      (* Upgrade to Git format, if needed *)
      master Irmin.Task.none t >>= fun master ->
      head (master ()) >>= function
      | Some _ -> Lwt.return t          (* The new Git-format store is ready *)
      | None ->
        (* Either this is a new deployment or we're upgrading from an older version.
           Try a migration. *)
        Old.Repo.create config >>= fun old ->
        Old.master Irmin.Task.none old >>= fun old_master ->
        let old_master = old_master () in
        Old.head old_master >>= function
        | None -> Lwt.return t          (* New deployment *)
        | Some old_head ->
          (* Migration needed *)
          Old.history old_master >>= fun history ->
          let commits = Topo.fold (fun n acc -> n :: acc) history [] |> List.rev in
          let n_commits = List.length commits in
          let replacements = Hashtbl.create n_commits in
          let old_commits = Old.Private.Repo.commit_t old in
          let new_commits = Private.Repo.commit_t t in
          let i = ref 0 in
          commits |> Lwt_list.iter_s (fun c ->
              if !i mod 100 = 0 then Printf.printf "Migrating commit %d/%d\n%!" !i n_commits;
              incr i;
              Old.Private.Commit.read_exn old_commits c >>= fun cv ->
              let tree = Old.Private.Commit.Val.node cv in
              let parents = Old.Private.Commit.Val.parents cv |> List.map (Hashtbl.find replacements) in
              let task = Old.Private.Commit.Val.task cv in
              lwt_opt_map (migrate_tree t ~old) tree >>= fun tree ->
              let nc = Private.Commit.Val.create task ?node:tree ~parents in
              Private.Commit.add new_commits nc >>= fun new_id ->
              Hashtbl.add replacements c new_id;
              Lwt.return_unit
            ) >>= fun () ->
          let new_head = Hashtbl.find replacements old_head in
          print_endline "All commits migrated.";
          Private.Ref.compare_and_set (Private.Repo.ref_t t) ~test:None T.master ~set:(Some new_head) >>= fun ok ->
          if not ok then failwith "Failed to set new head in DB migration!";
          Lwt.return t
  end
end
