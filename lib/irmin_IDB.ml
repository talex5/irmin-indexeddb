(* Copyright (C) 2020, Thomas Leonard.
 * See the README file for details. *)

open Lwt.Infix
open Iridb_utils
open Js_of_ocaml

let db_name_key =
  Irmin.Private.Conf.(key "indexeddb_db_name" string "Irmin")

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

module Content_store (K : Irmin.Hash.S) (V : Irmin.Type.S) = struct
  type 'a t = Iridb_lwt.store
  type key = K.t
  type value = V.t

  let string_of_hash = Irmin.Type.to_string K.t

  let value_of_string s =
    match Irmin.Type.of_bin_string V.t s with
    | Ok x -> x
    | Error (`Msg m) -> failwith m

  let string_of_value = Irmin.Type.to_bin_string V.t

  let find t k =
    Iridb_lwt.get t (string_of_hash k) >|= function
    | None -> None
    | Some s -> Some (value_of_string s)

  let mem t k =
    Iridb_lwt.get t (string_of_hash k) >|= function
    | None -> false
    | Some _ -> true

  let unsafe_add t key value =
    let value = string_of_value value in
    Iridb_lwt.set t (string_of_hash key) value

  let add t value =
    let value = string_of_value value in
    let k = K.hash (fun add -> add value) in
    Iridb_lwt.set t (string_of_hash k) value >|= fun () -> k

  let batch t fn = fn t

  let close _ = Lwt.return_unit

  let v config =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    connect db_name >|= fun idb ->
    Iridb_lwt.store idb ao
end

module Branch_store (K: Irmin.Type.S) (V: Irmin.Type.S) = struct
  module W = Irmin.Private.Watch.Make(K)(V)

  module Key = K
  module Val = V

  type key = K.t
  type value = V.t
  type watch = W.watch

  let string_of_key = Irmin.Type.to_string K.t

  let key_of_string s =
    match Irmin.Type.of_string K.t s with
    | Ok k -> k
    | Error (`Msg m) -> failwith m

  type t = {
    r : Iridb_lwt.store;
    watch : W.t;
    prefix : string;
    notifications : Iridb_html_storage.t;
    mutable listener : (Dom.event_listener_id * int) option;
  }

  let v config =
    let db_name = Irmin.Private.Conf.get config db_name_key in
    connect db_name >|= fun idb ->
    let prefix = db_name ^ ".rw." in
    let watch = W.v () in
    let notifications = Iridb_html_storage.make () in
    let r = Iridb_lwt.store idb rw in
    { watch; r; prefix; notifications; listener = None }

  let string_of_hash = Irmin.Type.to_bin_string V.t
  let hash_of_string x =
    match Irmin.Type.of_bin_string V.t x with
    | Ok x -> x
    | Error (`Msg m) -> failwith m

  let find t k =
    Iridb_lwt.get t.r (string_of_key k) >|= function
    | None -> None
    | Some s -> Some (hash_of_string s)

  let mem t k =
    Iridb_lwt.get t.r (string_of_key k) >|= function
    | None -> false
    | Some _ -> true

  let list t =
    Iridb_lwt.bindings t.r >|=
    List.map (fun (k, _v) -> key_of_string k)

  let ref_listener t =
    match t.listener with
    | None ->
        let l =
          Iridb_html_storage.watch t.notifications ~prefix:t.prefix (fun key value ->
            let subkey = tail key (String.length t.prefix) in
            let ir_key = key_of_string subkey in
            let value = value >|?= hash_of_string in
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
    | None -> Iridb_html_storage.remove t.notifications (t.prefix ^ string_of_key k)
    | Some v -> Iridb_html_storage.set t.notifications (t.prefix ^ string_of_key k) (string_of_hash v)
    end;
    (* Notify this tab *)
    W.notify t.watch k new_value

  let set t k value =
    (* Log.warn "Non-atomic update called!"; *)
    string_of_hash value
    |> Iridb_lwt.set t.r (string_of_key k) >>= fun () ->
    notify t k (Some value)

  let remove t k =
    (* Log.warn "Non-atomic remove called!"; *)
    Iridb_lwt.remove t.r (string_of_key k) >>= fun () ->
    notify t k None

  let test_and_set t k ~test ~set =
    let pred old =
      match old, test with
      | None, None -> true
      | Some old, Some expected -> old = string_of_hash expected
      | _ -> false in
    let new_value = set >|?= string_of_hash in
    Iridb_lwt.compare_and_set t.r (string_of_key k) ~test:pred ~new_value >>= function
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

  let close _ = Lwt.return_unit
end

let config db_name = Irmin.Private.Conf.singleton db_name_key db_name
