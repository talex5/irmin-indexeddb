open Lwt

module I = Irmin.Basic(Irmin_IDB.Make)(Irmin.Contents.String)

let key = ["key"]

let make_task s =
  let date = Unix.time () |> Int64.of_float in
  Irmin.Task.create ~date ~owner:"User" s

let die fmt =
  Printf.ksprintf failwith fmt

let db_name = "Irmin_IndexedDB_test"

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

  Lwt.catch (fun () ->
    print "Irmin-IndexedDB test";

    print "Deleting any previous test database...";
    Iridb_lwt.delete_database db_name >>= fun () ->

    let config = Irmin_IDB.config db_name in
    I.create config make_task >>= fun store ->
    let store = store "test" in
    print "Created basic store. Checking it is empty...";
    I.list store [] >>= expect ~fmt:key_list [] >>= fun () ->

    I.update store key "value" >>= fun () ->
    print "Added test item. Reading it back...";
    I.read_exn store key >>= expect_str "value" >>= fun () ->

    print "Listing contents...";
    I.list store [] >>= expect ~fmt:key_list [key] >>= fun () ->

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
