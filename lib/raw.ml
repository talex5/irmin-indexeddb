(* Copyright (C) 2015, Thomas Leonard.
 * See the README file for details. *)

open Lwt
open Js_of_ocaml
module Lwt_js = Js_of_ocaml_lwt.Lwt_js

type db = Js_api.database Js.t
type store_name = Js.js_string Js.t

type store = {
  db : db;
  store_name : store_name;

  (* We reuse transactions where possible for performance.
   * This does mean that if any read fails then the others will hang, but we treat any
   * read failing as a fatal error anyway. *)
  mutable ro_trans : (Js_api.transaction Js.t * (exn -> unit) list ref) option;
}

type key = string
type db_name = string
type db_upgrader = Js_api.database Js.t
let store_name = Js.string

let opt_string x ~if_missing =
  Js.Optdef.case x
    (fun () -> if_missing)
    (fun x -> Js.to_string x)

exception AbortError

let idb_error typ (event:Js_api.request Js_api.errorEvent Js.t) =
  let failure msg = Failure (Printf.sprintf "IndexedDB operation (%s) failed: %s" typ msg) in
  Js.Opt.case event##.target
    (fun () -> failure "(missing target on error event)")
    (fun target ->
      Js.Opt.case target##.error
        (fun () -> failure "(missing error on request)")
        (fun error ->
          let name = opt_string error##.name ~if_missing:"(no name)" in
          let message = opt_string error##.message ~if_missing:"(no message)" in
          let code = Js.Optdef.get error##.code (fun () -> 0) in
          if name = "AbortError" then AbortError
          else failure (Printf.sprintf "%s: %s (error code %d)" name message code)
        )
    )

let get_factory () =
  let factory : Js_api.factory Js.t Js.Optdef.t = (Obj.magic Dom_html.window)##.indexedDB in
  Js.Optdef.get factory
    (fun () -> failwith "IndexedDB not available")

let make db_name ~version ~init =
  let factory = get_factory () in
  let request = factory##_open (Js.string db_name) version in
  let t, set_t = Lwt.wait () in
  request##.onblocked := Dom.handler (fun _event ->
    print_endline "Waiting for other IndexedDB users to close their connections before upgrading schema version.";
    Js._true
  );
  request##.onupgradeneeded := Dom.handler (fun event ->
    try
      let old_version = event##.oldVersion in
      init ~old_version request##.result;
      Js._true
    with ex ->
      (* Firefox throws the exception away and returns AbortError instead, so save it here. *)
      Lwt.wakeup_exn set_t ex;
      raise ex
  );
  request##.onerror := Dom.handler (fun event ->
    begin match Lwt.state t, idb_error "open" event with
    | Fail _, AbortError -> ()   (* Already reported a better exception *)
    | _, ex -> Lwt.wakeup_exn set_t ex
    end;
    Js._true
  );
  request##.onsuccess := Dom.handler (fun _event ->
    Lwt.wakeup set_t request##.result;
    Js._true
  );
  t

let close db =
  db##close

let delete_database db_name =
  let factory = get_factory () in
  let request = factory##deleteDatabase (Js.string db_name) in
  let t, set_t = Lwt.wait () in
  request##.onerror := Dom.handler (fun _event ->
    Lwt.wakeup_exn set_t (Failure "Error trying to delete IndexedDB database");
    Js._true
  );
  request##.onsuccess := Dom.handler (fun _event ->
    Lwt.wakeup set_t ();
    Js._true
  );
  t

let store db store_name = { db; store_name; ro_trans = None }

let create_store db name =
  db##createObjectStore name |> ignore

let delete_store db name =
  db##deleteObjectStore name

let rec trans_ro (t:store) setup =
  let r, set_r = Lwt.wait () in
  match t.ro_trans with
  | None ->
      let breakers = ref [Lwt.wakeup_exn set_r] in
      let trans = t.db##transaction (Js.array [| t.store_name |]) (Js.string "readonly") in
      t.ro_trans <- Some (trans, breakers);
      trans##.onerror := Dom.handler (fun event ->
        t.ro_trans <- None;
        let ex = idb_error "RO" event in
        if ex = AbortError then
          print_endline "IndexedDB transaction failed (Safari bug?): will wait and retry";
        !breakers |> List.iter (fun b -> b ex);
        Js._true
      );
      trans##.oncomplete := Dom.handler (fun _event ->
        t.ro_trans <- None;
        Js._true
      );
      setup (trans##objectStore t.store_name) set_r;
      r
  | Some (trans, breakers) ->
      (* Seems we can get here when a transaction is done but oncomplete hasn't been called,
       * so retry if we get an error. *)
      try
        setup (trans##objectStore t.store_name) set_r;
        breakers := Lwt.wakeup_exn set_r :: !breakers;
        r
      with _ex ->
        t.ro_trans <- None;
        trans_ro t setup

(* On Safari, transactions can fail unpredictably, so wrap [trans_ro] with auto-retry.
 * See: https://github.com/talex5/cuekeeper/issues/9 *)
let trans_ro t setup =
  let rec retry delay =
    Lwt.catch (fun () -> trans_ro t setup)
      (function
        | AbortError -> Lwt_js.sleep (Random.float delay) >>= fun () -> retry (delay *. 1.2)
        | ex -> fail ex) in
  retry 1.0

let trans_rw t setup =
  let r, set_r = Lwt.wait () in
  let trans = t.db##transaction (Js.array [| t.store_name |]) (Js.string "readwrite") in
  trans##.onerror := Dom.handler (fun event ->
    Lwt.wakeup_exn set_r (idb_error "RW" event);
    Js._true
  );
  trans##.onabort := Dom.handler (fun event ->
    Lwt.wakeup_exn set_r (idb_error "RW" event);
    Js._true
  );
  trans##.oncomplete := Dom.handler (fun _event ->
    Lwt.wakeup set_r ();
    Js._true
  );
  setup (trans##objectStore t.store_name);
  r

let bindings t =
  let bindings = ref [] in
  trans_ro t
    (fun store set_r ->
      let request = store##openCursor in
      request##.onsuccess := Dom.handler (fun _event ->
        Js.Opt.case request##.result
          (fun () -> Lwt.wakeup set_r !bindings)
          (fun cursor ->
            let key = cursor##.key |> Js.to_string in
            let value = cursor##.value |> Js.to_string |> Utf8_codec.decode in
            bindings := (key, value) :: !bindings;
            cursor##continue
          );
        Js._true
      )
    )

let set t key value =
  trans_rw t (fun store ->
    store##put (Js.string (Utf8_codec.encode value)) (Js.string key) |> ignore
  )

let remove t key =
  trans_rw t
    (fun store ->
      store##delete (Js.string key) |> ignore
    )

let clear t =
  trans_rw t
    (fun store ->
      store##clear () |> ignore
    )

let get t key =
  trans_ro t
    (fun store set_r ->
      let request = store##get (Js.string key) in
      request##.onsuccess := Dom.handler (fun _event ->
        Js.Optdef.case request##.result
          (fun () -> None)
          (fun s -> Some (Js.to_string s |> Utf8_codec.decode))
        |> Lwt.wakeup set_r;
        Js._true
      )
    )

let compare_and_set t key ~test ~new_value =
  let result = ref None in
  let key = Js.string key in
  trans_rw t
    (fun store ->
      let request = store##get key in
      request##.onsuccess := Dom.handler (fun _event ->
          let actual =
            Utils.option_map (fun x -> Js.to_string x |> Utf8_codec.decode)
              (Js.Optdef.to_option request##.result)
        in
        if test actual then (
          begin match new_value with
          | None -> store##delete key |> ignore
          | Some new_value -> store##put (Js.string (Utf8_codec.encode new_value)) key |> ignore end;
          result := Some true
        ) else (
          result := Some false
        );
        Js._true
      )
    )
  >|= fun () ->
  match !result with
  | None -> failwith "Transaction completed, but no result!"
  | Some x -> x
