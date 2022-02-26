open Lwt
open Js_of_ocaml
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
module Raw = Irmin_indexeddb.Raw

module Schema = Irmin.Schema.KV (Irmin.Contents.String)

(* A Git-format store. This data can be exported and used with the regular Git
   tools. It can also read data produced by older versions of irmin-indexeddb. *)
module I = Irmin_git.Generic_KV(Irmin_indexeddb.Content_store)(Irmin_indexeddb.Branch_store)
module IStore = I.Make(Irmin.Contents.String)    (* (Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String) *)

(* An Irmin-format store. This allows storing custom metadata or using
   different hash functions, but is not compatible with the Git tools or with
   databases created by older versions of irmin-indexeddb. *)
module Plain = Irmin.Maker(Irmin_indexeddb.Content_store)(Irmin_indexeddb.Branch_store)
module PStore = Plain.Make(Schema)    (* (Irmin.Metadata.None)(Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)(Irmin.Hash.SHA256) *)

let key = ["key"]

let make_task s =
  let date = Unix.time () |> Int64.of_float in
  IStore.Info.v ~author:"User" ~message:s date

let die fmt =
  Fmt.kstr failwith fmt

let keys_of_store (type t) (module Store : Irmin.KV with type t = t) (t : t) =
  Store.list t []
  >|= List.map (fun (k, subtree) ->
      let kind =
        match Store.Tree.destruct subtree with
        | `Node _ -> `Node
        | `Contents _ -> `Contents
      in
      (k, kind))

let db_name = "Irmin_IndexedDB_test"
let upgrade_db_name = "Irmin_IndexedDB_t2"
let import_db_name = "Irmin_IndexedDB_t3"
let plain_db_name = "Irmin_IndexedDB_test_plain"

let start main =
  let document = Dom_html.document in
  let print fmt =
    let add msg =
      let node = document##createTextNode (Js.string (msg ^ "\n")) in
      Dom.appendChild main node |> ignore in
    Fmt.kstr add fmt in

  let expect ~fmt expected actual =
    if expected = actual then (
      print "Got %a, as expected" fmt expected;
      Lwt.return_unit
    ) else (
      print "Got %a, but expected %a!" fmt actual fmt expected;
      failwith "Tests FAILED"
    )
  in

  let expect_str = expect ~fmt:Fmt.(quote string) in
  let key_list f xs =
    let pp_item f (step, _) = Fmt.string f step in
    Fmt.pf f "[%a]"
      (Fmt.(list ~sep:(any ",")) pp_item) xs in

  let dump_bindings db store_name =
    let store_id = Raw.store_name store_name in
    let store = Raw.store db store_id in
    print "let %s = [" store_name;
    Raw.bindings store >|= fun bindings ->
    bindings |> List.iter (fun (name, value) ->
      print "  %S, %S;" name value
    );
    print "]" in

  let load_bindings db store_name bindings =
    let store_id = Raw.store_name store_name in
    let store = Raw.store db store_id in
    bindings |> Lwt_list.iter_s (fun (name, value) ->
      Raw.set store name value
    ) in

  Lwt.catch (fun () ->
    print "Irmin-IndexedDB test";

    print "Deleting any previous test databases...";
    Raw.delete_database db_name >>= fun () ->
    Raw.delete_database upgrade_db_name >>= fun () ->
    Raw.delete_database import_db_name >>= fun () ->
    Raw.delete_database plain_db_name >>= fun () ->

    let info () = PStore.Info.v ~message:"Test message" ~author:"Test <example.com>" 0L in
    begin
      let config = Irmin_indexeddb.config plain_db_name in
      PStore.Repo.v config >>= PStore.main >>= fun store ->
      print "Created Irmin-format basic store. Checking it is empty...";
      keys_of_store (module PStore) store >>= expect ~fmt:key_list [] >>= fun () ->
      PStore.set_exn ~info store key "value" >>= fun () ->
      print "Added test item. Reading it back...";
      PStore.get store key >>= expect_str "value" >>= fun () ->

      print "Listing contents...";
      keys_of_store (module PStore) store >>= expect ~fmt:key_list ["key", `Contents] >>= fun () ->

      PStore.Head.find store >>= function
      | None -> assert false
      | Some head ->
      print "Head: %a" PStore.Commit.pp_hash head;
      expect ~fmt:Fmt.(quote string) "Test message" @@ PStore.Info.message @@ PStore.Commit.info head >>= fun () ->
      PStore.set_exn ~info store key "value3" >>= fun () ->
      PStore.history store >>= fun hist ->
      PStore.History.iter_succ (fun head ->
        print "Parent: %a" PStore.Commit.pp_hash head
      ) hist head;

      print "Dumping DB contents... (ignore _git suffix)";

      Raw.make plain_db_name ~version:4 ~init:(fun ~old_version:_ _ -> assert false) >>= fun db ->
      dump_bindings db "ao_git" >>= fun () ->
      dump_bindings db "rw_git" >|= fun () ->
      Raw.close db
    end >>= fun () ->

    let info () = IStore.Info.v ~message:"Test message" ~author:"Test <example.com>" 0L in

    begin
      let config = Irmin_indexeddb.config db_name in
      IStore.Repo.v config >>= IStore.main >>= fun store ->
      print "Created Git-format basic store. Checking it is empty...";
      keys_of_store (module IStore) store >>= expect ~fmt:key_list [] >>= fun () ->
      IStore.set_exn ~info store key "value" >>= fun () ->
      print "Added test item. Reading it back...";
      IStore.get store key >>= expect_str "value" >>= fun () ->

      print "Listing contents...";
      keys_of_store (module IStore) store >>= expect ~fmt:key_list ["key", `Contents] >>= fun () ->

      IStore.Head.find store >>= function
      | None -> assert false
      | Some head ->
      print "Head: %a" IStore.Commit.pp_hash head;
      expect ~fmt:Fmt.(quote string) "Test message" @@ IStore.Info.message @@ IStore.Commit.info head >>= fun () ->
      IStore.set_exn ~info:info store key "value3" >>= fun () ->
      IStore.history store >>= fun hist ->
      IStore.History.iter_succ (fun head ->
        print "Parent: %a" IStore.Commit.pp_hash head
      ) hist head;

      print "Dumping DB contents...";

      Raw.make db_name ~version:4 ~init:(fun ~old_version:_ _ -> assert false) >>= fun db ->
      dump_bindings db "ao_git" >>= fun () ->
      dump_bindings db "rw_git" >|= fun () ->
      Raw.close db
    end >>= fun () ->

    print "Testing ability to read v3 format db";
    begin
      print "Importing old db dump...";
      let init ~old_version upgrader =
        assert (old_version = 0);
        Raw.(create_store upgrader (store_name "ao"));
        Raw.(create_store upgrader (store_name "rw"));
        Raw.(create_store upgrader (store_name "ao_git"));
        Raw.(create_store upgrader (store_name "rw_git")) in
      Raw.make upgrade_db_name ~version:3 ~init >>= fun db ->
      load_bindings db "ao" V3_db.ao >>= fun () ->
      load_bindings db "rw" V3_db.rw >>= fun () ->
      load_bindings db "ao_git" V3_db.ao_git >>= fun () ->
      load_bindings db "rw_git" V3_db.rw_git >>= fun () ->
      Raw.close db;

      print "Opening old db...";
      let config = Irmin_indexeddb.config upgrade_db_name in
      IStore.Repo.v config >>= fun up_repo ->
      IStore.main up_repo >>= fun up_store ->
      IStore.get up_store key >>= expect_str "value2" >>= fun () ->

      print "Exporting old db...";
      IStore.Repo.export up_repo >>= fun slice ->
      IStore.Head.find up_store >>= function
      | None -> assert false
      | Some head ->
      return (slice, head)
    end >>= fun (slice, head) ->

    begin
      Raw.make upgrade_db_name ~version:4 ~init:(fun ~old_version:_ _ -> assert false) >>= fun db ->
      dump_bindings db "ao_git" >>= fun () ->
      dump_bindings db "rw_git"
    end >>= fun () ->

    begin
      let config = Irmin_indexeddb.config import_db_name in
      IStore.Repo.v config >>= fun repo ->
      IStore.main repo >>= fun store ->
      print "Created new store. Checking it is empty...";
      IStore.list store [] >>= expect ~fmt:key_list [] >>= fun () ->

      print "Importing from bundle...";
      IStore.Repo.import repo slice >>= function
      | Error (`Msg m) -> die "Error importing slice: %s" m
      | Ok () ->

      IStore.Head.fast_forward store head >>= function
      | Error _ -> die "fast_forward_head failed"
      | Ok () ->
      print "Checking import worked...";
      keys_of_store (module IStore) store >>= expect ~fmt:key_list ["key", `Contents]
    end >>= fun () ->

    print "Success!";
    return ()
  ) (fun ex ->
    print "ERROR: %s" (Printexc.to_string ex);
    raise ex
  )

let () =
  match Dom_html.tagged (Dom_html.getElementById "main") with
  | Dom_html.Pre main -> Lwt_js_events.async (fun () -> start main)
  | _ -> failwith "Bad 'main' element"
