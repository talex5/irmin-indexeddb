open Lwt.Infix
open Irmin

module KV = Irmin_IDB.KV(Irmin.Contents.String)

let key = ["key"]
let last l = List.hd (List.rev l)

let info () =
  let date = Unix.time () |> Int64.of_float in
  Irmin.Info.v ~date ~author:"User" "Test."

let die fmt = Fmt.kstrf failwith fmt

let db_name = "Irmin_IndexedDB_test"
let upgrade_db_name = "Irmin_IndexedDB_t2"
let import_db_name = "Irmin_IndexedDB_t3"

module type DB = sig
  val ao : unit -> (string * string) list Lwt.t
  val rw : unit -> (string * string) list Lwt.t
end

module Make(I : Irmin.KV with type contents = string) (Db : DB) = struct
  let start ~version main =
    let document = Dom_html.document in

    let print fmt =
      let add msg =
        let node = document##createTextNode (Js.string (msg ^ "\n")) in
        Dom.appendChild main node |> ignore in
      Fmt.kstrf add fmt
    in

    print "Testing Irmin %s format store." version;

    let expect ~fmt expected actual =
      if expected = actual then print "Got %a, as expected" fmt expected
      else die "Got %a, but expected %a!" fmt actual fmt expected;
      Lwt.return () in

    let expect_str =
      expect ~fmt:(fun ppf x -> Fmt.string ppf (String.escaped x))
    in
    let key_list ppf xs =
      let pp_typ = Type.pp_json I.kind_t in
      Fmt.pf ppf "[%a]" Fmt.(Dump.list (Dump.pair I.Key.pp_step pp_typ)) xs
    in

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
        I.Repo.v config >>= I.master >>= fun store ->
        print "Created basic store. Checking it is empty...";
        I.list store [] >>= expect ~fmt:key_list [] >>= fun () ->

        I.set store ~info key "value" >>= fun () ->
        print "Added test item. Reading it back...";
        I.get store key >>= expect_str "value" >>= fun () ->

        print "Listing contents...";
        I.list store [] >>= expect ~fmt:key_list [last key, `Contents] >>= fun () ->

        I.Head.get store >>= fun head ->
        print "Head: %a" I.Commit.pp head;
        I.set store ~info key "value2" >>= fun () ->
        I.history store >>= fun hist ->
        I.History.iter_succ (fun head ->
          print "Parent: %a" I.Commit.pp head
        ) hist head;

        print "Dumping DB contents...";

        Iridb_lwt.make db_name ~version:2 ~init:(fun _ -> assert false) >>= fun db ->
        dump_bindings db "ao" >>= fun () ->
        dump_bindings db "rw" >|= fun () ->
        Iridb_lwt.close db
      end >>= fun () ->

      print "Testing ability to read saved db in %s format" version;
      begin
        let init upgrader =
          Iridb_lwt.(create_store upgrader (store_name "ao"));
          Iridb_lwt.(create_store upgrader (store_name "rw")) in
        Iridb_lwt.make upgrade_db_name ~version:2 ~init >>= fun db ->
        Db.ao () >>= fun ao ->
        load_bindings db "ao" ao >>= fun () ->
        Db.rw () >>= fun rw ->
        load_bindings db "rw" rw >>= fun () ->
        Iridb_lwt.close db;

        let config = Irmin_IDB.config upgrade_db_name in
        I.Repo.v config >>= fun up_repo ->
        I.Repo.export up_repo >>= fun i ->
        print "%a" Irmin.Type.(pp_json I.Private.Slice.t) i;
        I.master up_repo >>= fun up_store ->
        I.get up_store key >>= expect_str "value2" >>= fun () ->

        print "Exporting old db...";
        I.Repo.export up_repo >>= fun slice ->
        I.Head.get up_store >|= fun head ->
        (slice, head)
      end >>= fun (slice, head) ->

      begin
        let config = Irmin_IDB.config import_db_name in
        I.Repo.v config >>= fun repo ->
        I.master repo >>= fun store ->
        print "Created new store. Checking it is empty...";
        I.list store [] >>= expect ~fmt:key_list [] >>= fun () ->

        print "Importing from bundle...";
        I.Repo.import repo slice >>= function
        | Error (`Msg e) -> die "Error importing slice: %s" e
        | Ok () ->

        I.Head.fast_forward store head >>= function
        | Error e ->
          die "fast_forward_head failed: %a" Type.(pp_json I.ff_error_t) e
        | Ok ()   ->
        print "Checking import worked...";
        I.list store [] >>= expect ~fmt:key_list [last key, `Contents]
      end >>= fun () ->

      print "Success!\n\n";
      Lwt.return ()
    ) (fun ex ->
      print "ERROR: %s" (Printexc.to_string ex);
      raise ex
    )
end

module Compat = Irmin_compat.From_v0_11(KV)

module Test_v0_11 = Make(KV)(struct
    let db_name = "Irmin_IndexedDB_upgrade"
    let config = Irmin_IDB.config db_name

    let init = lazy (
      print_endline "Import/Export";
      KV.Repo.v config >>= fun repo ->
      Compat.import repo (module V2_db) >>= fun () ->
      Compat.export repo
    )

    let ao () = Lazy.force init >|= fun (module DB) -> DB.ao
    let rw () = Lazy.force init >|= fun (module DB) -> DB.rw

  end)

module Test_v1 = Make(KV)(struct
    let ao () = Lwt.return V3_db.ao
    let rw () = Lwt.return V3_db.rw
  end)

let () =
  match Dom_html.tagged (Dom_html.getElementById "main") with
  | Dom_html.Pre main -> Lwt_js_events.async (fun () ->
      Test_v0_11.start ~version:"0.11" main >>= fun () ->
      Test_v1.start    ~version:"1.0"  main
    )
  | _ -> failwith "Bad 'main' element"
