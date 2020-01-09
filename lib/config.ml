(* Copyright (C) 2020, Thomas Leonard.
   See the README file for details. *)

let db_name_key =
  Irmin.Private.Conf.(key "indexeddb_db_name" string "Irmin")

(* These were the stores used in schema version 2 to store the data. *)
let ao_old = Raw.store_name "ao"
let rw_old = Raw.store_name "rw"

(* These stores were used in schema version 3 to store the data after
   migrating it to Git format. We now use them even for non-Git-format data,
   since with Irmin 2.0 we don't know how the store will be used. *)
let ao = Raw.store_name "ao_git"
let rw = Raw.store_name "rw_git"

exception Format_too_old of [`Irmin_0_10]

let version = 4
let connect db_name =
  Raw.make db_name ~version ~init:(fun ~old_version upgrader ->
    match old_version with
    | 0 ->
      Raw.create_store upgrader ao;
      Raw.create_store upgrader rw
    | 2 ->
      raise (Format_too_old `Irmin_0_10)
    | 3 ->
      (* Remove old stores from 2->3 migration. *)
      Raw.delete_store upgrader ao_old;
      Raw.delete_store upgrader rw_old
    | _ ->
      failwith "Attempt to upgrade from unknown schema version!"
  )

let v db_name = Irmin.Private.Conf.singleton db_name_key db_name
