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
    Lwt_list.iter_p (fun (k, v) ->
        fn (K.of_hum k) (fun () -> return (Tc.read_string (module V) v))
      )
end

module AO (K: Irmin.Hash.S) (V: Tc.S0) = struct
  include RO(K)(V)

  let create config =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    connect db_name ~version >>= fun idb ->
    return (Iridb_lwt.store idb ao)

  let add t value =
    let k = Tc.write_cstruct (module V) value |> K.digest in
    let v = Tc.write_string (module V) value in
    Iridb_lwt.set t (K.to_hum k) v >|= fun () -> k
end

module RW (K: Irmin.Hum.S) (V: Tc.S0) = struct
  module W = Irmin.Private.Watch.Make(K)(V)
  module R = RO(K)(V)

  type watch = W.watch

  type t = {
    r : R.t;
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
    connect db_name ~version >>= fun idb ->
    let r = Iridb_lwt.store idb rw in
    return { watch; r; prefix; notifications; listener = None }

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
      | Some old, Some expected -> Tc.read_string (module V) old |> V.equal expected
      | _ -> false in
    let new_value = set >|?= Tc.write_string (module V) in
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

  let iter t = R.iter t.r
  let mem t = R.mem t.r
  let read t = R.read t.r
  let read_exn t = R.read_exn t.r
  type value = R.value
  type key = R.key
end

let config db_name = Irmin.Private.Conf.singleton db_name_key db_name

module Make_v0_11 (C: Irmin.Contents.S) (T: Irmin.Ref.S) (H: Irmin.Hash.S) =
  Irmin.Make(AO)(RW)(C)(T)(H)

module Make_v0_10
    (C: Irmin.Contents.S)
    (R: Irmin.Ref.S)
    (H: Irmin.Hash.S) =
struct
  (* Irmin 0.11's own [Metadata.None] writes a single zero byte, which changed the binary format.
     This one is compatible with Irmin 0.10. *)
  module No_metadata = struct
    include Tc.Bin_prot0(struct
      type t = unit
      let compare _ _ = 0
      let to_json = Ezjsonm.unit
      let of_json = Ezjsonm.get_unit
      let bin_size_t () = 0
      let bin_write_t _b ~pos () = pos
      let bin_read_t _b ~pos_ref:_ = ()
    end)

    let to_hum () = "unit"
    let of_hum = function "unit" -> () | _ -> failwith "Unit.of_hum"
    let default = ()
    let merge ~old:_ () () = Lwt.return (`Ok ())
  end

  (* This is the same as the Irmin 0.11 version, except that it uses an option for the root node. *)
  module Make_commit (C: Tc.S0) (Node: Irmin.Private.Node.STORE) = struct
    module N = Node.Key
    module T = Irmin.Task
    type node = N.t
    type commit = C.t

    type t = {
      node   : N.t;
      parents: C.t list;
      task : T.t;
    }

    let parents t = t.parents
    let node t = t.node
    let task t = t.task
    let create task ~node ~parents = { node; parents; task }

    let to_json t =
      `O [
        ("node"   , N.to_json t.node);
        ("parents", Ezjsonm.list C.to_json t.parents);
        ("task"   , T.to_json t.task);
      ]

    let of_json j =
      let node    = Ezjsonm.find j ["node"]    |> N.of_json in
      let parents = Ezjsonm.find j ["parents"] |> Ezjsonm.get_list C.of_json in
      let task    = Ezjsonm.find j ["task"]    |> T.of_json in
      { node; parents; task }

    module X = Tc.Triple(Tc.Option(N))(Tc.List(C))(T)

    let empty_hash =
      let len = Node.Val.(size_of empty) in
      let buf = Cstruct.create len in
      let rest = Node.Val.(write empty) buf in
      assert (Cstruct.len rest = 0);
      Node.Key.digest buf

    let explode t =
      let node =
        if Node.Key.equal t.node empty_hash then None
        else Some t.node
      in
      node, t.parents, t.task

    let implode (node, parents, task) =
      let node =
        match node with
        | None -> empty_hash
        | Some n -> n
      in
      { node; parents; task }

    let x = Tc.biject (module X) implode explode

    let hash = Tc.hash x
    let compare = Tc.compare x
    let equal = Tc.equal x

    let size_of = Tc.size_of x
    let write = Tc.write x
    let read = Tc.read x

  end

  module X = struct
    module XContents = struct
      include AO(H)(C)
      module Key = H
      module Val = C
    end
    module Contents = Irmin.Contents.Store(XContents)
    module Node = struct
      module AO = struct
        module Key = H
        module Val = Irmin.Private.Node.Make (H)(H)(C.Path)(No_metadata)
        include AO (Key)(Val)
      end
      include Irmin.Private.Node.Store(Contents)(AO)
      let create = AO.create
    end
    module Commit = struct
      module AO = struct
        module Key = H
        module Val = Make_commit (H)(Node)
        include AO (Key)(Val)
      end
      include Irmin.Private.Commit.Store(Node)(AO)
      let create = AO.create
    end
    module Ref = struct
      module Key = R
      module Val = H
      include RW (Key)(Val)
    end
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = Irmin.Private.Sync.None(H)(R)
    module Repo = struct
      type t = {
        config: Irmin.Private.Conf.t;
        contents: Contents.t;
        node: Node.t;
        commit: Commit.t;
        ref_store: Ref.t;
      }
      let ref_t t = t.ref_store
      let commit_t t = t.commit
      let node_t t = t.node
      let contents_t (t:t) = t.contents

      let create config =
        XContents.create config >>= fun contents ->
        Node.create config      >>= fun node ->
        Commit.create config    >>= fun commit ->
        Ref.create config       >>= fun ref_store ->
        let node = contents, node in
        let commit = node, commit in
        return { contents; node; commit; ref_store; config }
    end
  end
  include Irmin.Make_ext(X)
end
