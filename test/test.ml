open Lwt
open Js_of_ocaml
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events
module Raw = Irmin_indexeddb.Raw

(* A Git-format store. This data can be exported and used with the regular Git
   tools. It can also read data produced by older versions of irmin-indexeddb. *)
module I = Irmin_git.Generic(Irmin_indexeddb.Content_store)(Irmin_indexeddb.Branch_store)
    (Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)

(* An Irmin-format store. This allows storing custom metadata or using
   different hash functions, but is not compatible with the Git tools or with
   databases created by older versions of irmin-indexeddb. *)
module Plain = Irmin.Make(Irmin_indexeddb.Content_store)(Irmin_indexeddb.Branch_store)
    (Irmin.Metadata.None)(Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)(Irmin.Hash.SHA256)

let key = ["key"]

let make_task s =
  let date = Unix.time () |> Int64.of_float in
  Irmin.Info.v ~date ~author:"User" s

let die fmt =
  Fmt.kstrf failwith fmt

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
      (Fmt.(list ~sep:(unit ",")) pp_item) xs in

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

    let info () = Irmin.Info.v "Test message" ~date:0L ~author:"Test <example.com>" in
    begin
      let config = Irmin_indexeddb.config plain_db_name in
      Plain.Repo.v config >>= Plain.master >>= fun store ->
      print "Created Irmin-format basic store. Checking it is empty...";
      keys_of_store (module Plain) store >>= expect ~fmt:key_list [] >>= fun () ->
      Plain.set_exn ~info store key "value" >>= fun () ->
      print "Added test item. Reading it back...";
      Plain.get store key >>= expect_str "value" >>= fun () ->

      print "Listing contents...";
      keys_of_store (module Plain) store >>= expect ~fmt:key_list ["key", `Contents] >>= fun () ->

      Plain.Head.find store >>= function
      | None -> assert false
      | Some head ->
      print "Head: %a" Plain.Commit.pp_hash head;
      expect ~fmt:Fmt.(quote string) "Test message" @@ Irmin.Info.message @@ Plain.Commit.info head >>= fun () ->
      Plain.set_exn ~info store key "value3" >>= fun () ->
      Plain.history store >>= fun hist ->
      Plain.History.iter_succ (fun head ->
        print "Parent: %a" Plain.Commit.pp_hash head
      ) hist head;

      print "Dumping DB contents... (ignore _git suffix)";

      Raw.make plain_db_name ~version:4 ~init:(fun ~old_version:_ _ -> assert false) >>= fun db ->
      dump_bindings db "ao_git" >>= fun () ->
      dump_bindings db "rw_git" >|= fun () ->
      Raw.close db
    end >>= fun () ->

    begin
      let config = Irmin_indexeddb.config db_name in
      I.Repo.v config >>= I.master >>= fun store ->
      print "Created Git-format basic store. Checking it is empty...";
      keys_of_store (module I) store >>= expect ~fmt:key_list [] >>= fun () ->
      I.set_exn ~info store key "value" >>= fun () ->
      print "Added test item. Reading it back...";
      I.get store key >>= expect_str "value" >>= fun () ->

      print "Listing contents...";
      keys_of_store (module I) store >>= expect ~fmt:key_list ["key", `Contents] >>= fun () ->

      I.Head.find store >>= function
      | None -> assert false
      | Some head ->
      print "Head: %a" I.Commit.pp_hash head;
      expect ~fmt:Fmt.(quote string) "Test message" @@ Irmin.Info.message @@ I.Commit.info head >>= fun () ->
      I.set_exn ~info store key "value3" >>= fun () ->
      I.history store >>= fun hist ->
      I.History.iter_succ (fun head ->
        print "Parent: %a" I.Commit.pp_hash head
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
      I.Repo.v config >>= fun up_repo ->
      I.master up_repo >>= fun up_store ->
      I.get up_store key >>= expect_str "value2" >>= fun () ->

      print "Exporting old db...";
      I.Repo.export up_repo >>= fun slice ->
      I.Head.find up_store >>= function
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
      I.Repo.v config >>= fun repo ->
      I.master repo >>= fun store ->
      print "Created new store. Checking it is empty...";
      I.list store [] >>= expect ~fmt:key_list [] >>= fun () ->

      print "Importing from bundle...";
      I.Repo.import repo slice >>= function
      | Error (`Msg m) -> die "Error importing slice: %s" m
      | Ok () ->

      I.Head.fast_forward store head >>= function
      | Error _ -> die "fast_forward_head failed"
      | Ok () ->
      print "Checking import worked...";
      keys_of_store (module I) store >>= expect ~fmt:key_list ["key", `Contents]
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
