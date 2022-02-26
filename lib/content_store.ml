(* Copyright (C) 2020, Thomas Leonard.
   See the README file for details. *)

open Lwt.Infix

module Make (K : Irmin.Hash.S) (V : Irmin.Type.S) = struct
  type 'a t = Raw.store
  type key = K.t
  type value = V.t

  let string_of_hash = Irmin.Type.to_string K.t

  let value_of_string =
    let value_of_string = Irmin.Type.(unstage (of_bin_string V.t)) in
    fun s ->
      match value_of_string s with
      | Ok x -> x
      | Error (`Msg m) -> failwith m

  let string_of_value = Irmin.Type.(unstage (to_bin_string V.t))

  let find t k =
    Raw.get t (string_of_hash k) >|= function
    | None -> None
    | Some s -> Some (value_of_string s)

  let mem t k =
    Raw.get t (string_of_hash k) >|= function
    | None -> false
    | Some _ -> true

  let unsafe_add t key value =
    let value = string_of_value value in
    Raw.set t (string_of_hash key) value

  let add t value =
    let pre_hash = Irmin.Type.(unstage (pre_hash V.t)) in
    let v = string_of_value value in
    let k = K.hash (pre_hash value) in
    Raw.set t (string_of_hash k) v >|= fun () -> k

  let batch t fn = fn t

  let clear t = Raw.clear t

  let close _ = Lwt.return_unit

  let v config =
    let db_name = Irmin.Backend.Conf.get config Config.db_name_key in
    Config.connect db_name >|= fun idb ->
    Raw.store idb Config.ao
end
