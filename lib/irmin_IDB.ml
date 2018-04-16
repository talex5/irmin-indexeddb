(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt.Infix
open Irmin
open Iridb_utils

let src = Logs.Src.create "irmin-idb" ~doc:"IndexedDB backend for Irmin"
module Log = (val Logs.src_log src : Logs.LOG)

let err_invalid n v e =
  raise (Invalid_argument (Printf.sprintf "%s.of_string %S: %s " n v e))

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

module Conv (X: Contents.Conv) = struct

  let to_string k = Fmt.to_to_string X.pp k

  let of_string n v = match X.of_string v with
    | Ok v           -> v
    | Error (`Msg e) -> err_invalid n v e

end

module RO (K: Contents.Conv) (V: Contents.Conv) = struct
  type key = K.t
  type value = V.t
  type t = Iridb_lwt.store

  module Key = Conv(K)
  module Val = Conv(V)

  let find n t k =
    Iridb_lwt.get t (Key.to_string k) >|= function
    | None   -> None
    | Some s -> Some (Val.of_string n s)

  let mem t k =
    Iridb_lwt.get t (Key.to_string k) >|= function
    | None   -> false
    | Some _ -> true

end

module AO (K: Hash.S) (V: Contents.Conv) = struct
  include RO(K)(V)
  let find = find "value"

  let v config =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    connect db_name ~version >|= fun idb ->
    Iridb_lwt.store idb ao

  let value_of_string v = Fmt.to_to_string V.pp v

  let add t value =
    let v = value_of_string value in
    let k = K.digest Type.string v in
    Iridb_lwt.set t (Key.to_string k) v >|= fun () -> k
end

module RW (K: Contents.Conv) (V: Contents.Conv) = struct
  module W = Irmin.Private.Watch.Make(K)(V)
  module R = RO(K)(V)
  module Key = R.Key
  module Val = R.Val

  type watch = W.watch

  type t = {
    r : R.t;
    watch : W.t;
    prefix : string;
    notifications : Iridb_html_storage.t;
    mutable listener : (Dom.event_listener_id * int) option;
  }

  let v config =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    let prefix = db_name ^ ".rw." in
    let watch = W.v () in
    let notifications = Iridb_html_storage.make () in
    connect db_name ~version >|= fun idb ->
    let r = Iridb_lwt.store idb rw in
    { watch; r; prefix; notifications; listener = None }

  let ref_listener t =
    match t.listener with
    | None ->
        let l =
          Iridb_html_storage.watch t.notifications ~prefix:t.prefix (fun key value ->
            let subkey = tail key (String.length t.prefix) in
            let ir_key = Key.of_string "ref" subkey in
            let value = value >|?= Val.of_string "hash" in
            Lwt.async (fun () -> W.notify t.watch ir_key value)
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
    | None   ->
      Iridb_html_storage.remove t.notifications (t.prefix ^ Key.to_string k)
    | Some v ->
      Iridb_html_storage.set t.notifications
        (t.prefix ^ Key.to_string k) (Val.to_string v)
    end;
    (* Notify this tab *)
    W.notify t.watch k new_value

  let set t k value =
    (* Log.warn "Non-atomic update called!"; *)
    Val.to_string value
    |> Iridb_lwt.set t.r (Key.to_string k) >>= fun () ->
    notify t k (Some value)

  let remove t k =
    (* Log.warn "Non-atomic remove called!"; *)
    Iridb_lwt.remove t.r (Key.to_string k) >>= fun () ->
    notify t k None

  let test_and_set t k ~test ~set =
    let pred old =
      match old, test with
      | None, None -> true
      | Some old, Some expected -> Type.equal V.t (Val.of_string "hash" old) expected
      | _ -> false in
    let new_value = set >|?= Val.to_string in
    Iridb_lwt.compare_and_set t.r (Key.to_string k) ~test:pred ~new_value
    >>= function
    | true -> notify t k set >|= fun () -> true
    | false -> Lwt.return false

  let watch t ?init cb =
    ref_listener t;
    W.watch t.watch ?init cb

  let unwatch t w =
    unref_listener t;
    W.unwatch t.watch w

  let watch_key t key ?init cb =
    ref_listener t;
    W.watch_key t.watch key ?init cb

  let list t =
    Iridb_lwt.bindings t.r >|=
    List.map (fun (k, _) -> Key.of_string "ref" k)

  let mem t = R.mem t.r
  let find t = R.find "hash" t.r
  type value = R.value
  type key = R.key
end

let config db_name = Irmin.Private.Conf.singleton db_name_key db_name

module Make = Irmin.Make(AO)(RW)

module KV (C: Irmin.Contents.S) =
  Make
    (Irmin.Metadata.None)
    (C)
    (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
