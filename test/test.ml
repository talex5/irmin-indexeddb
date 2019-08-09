open Lwt
open Js_of_ocaml

module I = Irmin_IDB.Make(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

let key = ["key"]

let make_task s =
  let date = Unix.time () |> Int64.of_float in
  Irmin.Task.create ~date ~owner:"User" s

let die fmt =
  Printf.ksprintf failwith fmt

let db_name = "Irmin_IndexedDB_test"
let upgrade_db_name = "Irmin_IndexedDB_t2"
let import_db_name = "Irmin_IndexedDB_t3"

let start main =
  let document = Dom_html.document in
  let print fmt =
    let add msg =
      let node = document##createTextNode (Js.string (msg ^ "\n")) in
      Dom.appendChild main node |> ignore in
    Printf.ksprintf add fmt in

  let expect ~fmt expected actual =
    if expected = actual then print "Got %a, as expected" fmt expected
    else die "Got %a, but expected %a!" fmt actual fmt expected;
    return () in

  let expect_str = expect ~fmt:(fun () x -> String.escaped x) in
  let key_list () xs = "[" ^ (xs |> List.map I.Key.to_hum |> String.concat ",") ^ "]" in

  let dump_bindings db store_name =
    let store_id = Iridb_lwt.store_name store_name in
    let store = Iridb_lwt.store db store_id in
    print "let %s = [" store_name;
    Iridb_lwt.bindings store >|= fun bindings ->
    bindings |> List.iter (fun (name, value) ->
      print "  %S, %S;" name value
    );
    print "]" in

  let load_bindings db store_name bindings =
    let store_id = Iridb_lwt.store_name store_name in
    let store = Iridb_lwt.store db store_id in
    bindings |> Lwt_list.iter_s (fun (name, value) ->
      Iridb_lwt.set store name value
    ) in

  Lwt.catch (fun () ->
    print "Irmin-IndexedDB test";

    print "Deleting any previous test databases...";
    Iridb_lwt.delete_database db_name >>= fun () ->
    Iridb_lwt.delete_database upgrade_db_name >>= fun () ->
    Iridb_lwt.delete_database import_db_name >>= fun () ->

    begin
      let config = Irmin_IDB.config db_name in
      I.Repo.create config >>= I.master make_task >>= fun store ->
      let store = store "test" in
      print "Created basic store. Checking it is empty...";
      I.list store [] >>= expect ~fmt:key_list [] >>= fun () ->

      I.update store key "value" >>= fun () ->
      print "Added test item. Reading it back...";
      I.read_exn store key >>= expect_str "value" >>= fun () ->

      print "Listing contents...";
      I.list store [] >>= expect ~fmt:key_list [key] >>= fun () ->

      I.head store >>= function
      | None -> assert false
      | Some head ->
      print "Head: %s" (Irmin.Hash.SHA1.to_hum head);
      I.update store key "value2" >>= fun () ->
      I.history store >>= fun hist ->
      I.History.iter_succ (fun head ->
        print "Parent: %s" (Irmin.Hash.SHA1.to_hum head)
      ) hist head;

      print "Dumping DB contents...";

      Iridb_lwt.make db_name ~version:2 ~init:(fun _ -> assert false) >>= fun db ->
      dump_bindings db "ao" >>= fun () ->
      dump_bindings db "rw" >|= fun () ->
      Iridb_lwt.close db
    end >>= fun () ->

    print "Testing ability to read v1 format db";
    begin
      let init upgrader =
        Iridb_lwt.(create_store upgrader (store_name "ao"));
        Iridb_lwt.(create_store upgrader (store_name "rw")) in
      Iridb_lwt.make upgrade_db_name ~version:2 ~init >>= fun db ->
      load_bindings db "ao" V1_db.ao >>= fun () ->
      load_bindings db "rw" V1_db.rw >>= fun () ->
      Iridb_lwt.close db;

      let config = Irmin_IDB.config upgrade_db_name in
      I.Repo.create config >>= fun up_repo ->
      I.master make_task up_repo >>= fun up_store ->
      let up_store = up_store "test" in
      I.read_exn up_store key >>= expect_str "value2" >>= fun () ->

      print "Exporting old db...";
      I.Repo.export up_repo >>= fun slice ->
      I.head up_store >>= function
      | None -> assert false
      | Some head ->
      return (slice, head)
    end >>= fun (slice, head) ->

    begin
      let config = Irmin_IDB.config import_db_name in
      I.Repo.create config >>= fun repo ->
      I.master make_task repo >>= fun store ->
      let store = store "test" in
      print "Created new store. Checking it is empty...";
      I.list store [] >>= expect ~fmt:key_list [] >>= fun () ->

      print "Importing from bundle...";
      I.Repo.import repo slice >>= function
      | `Error -> die "Error importing slice"
      | `Ok ->

      I.fast_forward_head store head >>= function
      | false -> die "fast_forward_head failed"
      | true ->
      print "Checking import worked...";
      I.list store [] >>= expect ~fmt:key_list [key]
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
