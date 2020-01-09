(* Copyright (C) 2020, Thomas Leonard.
   See the README file for details. *)

open Lwt.Infix
open Js_of_ocaml

module Make (K: Irmin.Type.S) (V: Irmin.Type.S) = struct
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
    r : Raw.store;
    watch : W.t;
    prefix : string;
    notifications : Html_storage.t;
    mutable listener : (Dom.event_listener_id * int) option;
  }

  let v config =
    let db_name = Irmin.Private.Conf.get config Config.db_name_key in
    Config.connect db_name >|= fun idb ->
    let prefix = db_name ^ ".rw." in
    let watch = W.v () in
    let notifications = Html_storage.make () in
    let r = Raw.store idb Config.rw in
    { watch; r; prefix; notifications; listener = None }

  let string_of_hash = Irmin.Type.to_bin_string V.t
  let hash_of_string x =
    match Irmin.Type.of_bin_string V.t x with
    | Ok x -> x
    | Error (`Msg m) -> failwith m

  let find t k =
    Raw.get t.r (string_of_key k) >|= function
    | None -> None
    | Some s -> Some (hash_of_string s)

  let mem t k =
    Raw.get t.r (string_of_key k) >|= function
    | None -> false
    | Some _ -> true

  let list t =
    Raw.bindings t.r >|=
    List.map (fun (k, _v) -> key_of_string k)

  let ref_listener t =
    match t.listener with
    | None ->
        let l =
          Html_storage.watch t.notifications ~prefix:t.prefix (fun key value ->
            let subkey = Utils.tail key (String.length t.prefix) in
            let ir_key = key_of_string subkey in
            let value = Utils.option_map hash_of_string value in
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
    | None -> Html_storage.remove t.notifications (t.prefix ^ string_of_key k)
    | Some v -> Html_storage.set t.notifications (t.prefix ^ string_of_key k) (string_of_hash v)
    end;
    (* Notify this tab *)
    W.notify t.watch k new_value

  let set t k value =
    (* Log.warn "Non-atomic update called!"; *)
    string_of_hash value
    |> Raw.set t.r (string_of_key k) >>= fun () ->
    notify t k (Some value)

  let remove t k =
    (* Log.warn "Non-atomic remove called!"; *)
    Raw.remove t.r (string_of_key k) >>= fun () ->
    notify t k None

  let test_and_set t k ~test ~set =
    let pred old =
      match old, test with
      | None, None -> true
      | Some old, Some expected -> old = string_of_hash expected
      | _ -> false in
    let new_value = Utils.option_map string_of_hash set in
    Raw.compare_and_set t.r (string_of_key k) ~test:pred ~new_value >>= function
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
