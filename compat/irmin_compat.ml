(*
 * Copyright (c) 2013-2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Thomas Leonard
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix

let tts = Fmt.to_to_string

module type DB = sig
  val ao : (string * string) list
  val rw : (string * string) list
end

type task = {
  date : int64;
  uid  : int64;
  owner: string;
  msgs : string list;
}

type 'h commit = {
  node   : 'h;
  parents: 'h list;
  task   : task;
}

type ('h, 'm, 's) tree =  ('s * [`Contents of ('h * 'm) | `Node of 'h]) list

module Task: Tc.S0 with type t = task = struct
  type t = task
  module X = Tc.Pair
      (Tc.Pair(Tc.Int64)(Tc.Int64))
      (Tc.Pair(Tc.String)(Tc.List(Tc.String)))

  let explode t = (t.date, t.uid), (t.owner, t.msgs)
  let implode ( (date, uid), (owner, msgs) ) = { date; uid; owner; msgs }

  let hash t = X.hash (explode t)
  let compare x y = X.compare (explode x) (explode y)
  let size_of x = X.size_of (explode x)
  let write x b = X.write (explode x) b
  let read b = implode (X.read b)
  let equal x y = X.equal (explode x) (explode y)
  let of_json x = implode (X.of_json x)
  let to_json x = X.to_json (explode x)
end

module Commit_v0_11 (H: Tc.S0):
  Tc.S0 with type t =  H.t commit =
struct
  type t = H.t commit
  module X = Tc.Triple(H)(Tc.List(H))(Task)
  let explode t = t.node, t.parents, t.task
  let implode (node, parents, task) = { node; parents; task }
  let x = Tc.biject (module X) implode explode
  let hash = Tc.hash x
  let compare = Tc.compare x
  let equal = Tc.equal x
  let size_of = Tc.size_of x
  let write = Tc.write x
  let read = Tc.read x
  let to_json = Tc.to_json x
  let of_json = Tc.of_json x
end

module Tree_v0_11
    (H: Tc.S0) (* hashes *)
    (M: Tc.S0) (* metadata *)
    (S: Tc.S0) (* step *)
  : Tc.S0 with type t = (H.t, M.t, S.t) tree
= struct
  module StepMap = Map.Make(S)

  type contents = H.t * M.t
  type node = H.t

  module ContentsMeta = Tc.Pair(H)(M)

  type x = [`Contents of contents | `Node of node ]

  module X: Tc.S0 with type t = x = struct

    type t = x

    let tag buf i =
      Cstruct.set_uint8 buf 0 i;
      Cstruct.shift buf 1

    let untag buf =
      Mstruct.get_uint8 buf

    let compare x y = match x, y with
      | `Contents x, `Contents y -> ContentsMeta.compare x y
      | `Node x    , `Node y     -> H.compare x y
      | `Contents _, _           -> 1
      | _ -> -1

    let equal x y = match x, y with
      | `Contents x, `Contents y -> ContentsMeta.equal x y
      | `Node x    , `Node y     -> H.equal x y
      | _ -> false

    let hash = Hashtbl.hash

    let to_json = function
      | `Contents c -> `O [ "contents", ContentsMeta.to_json c ]
      | `Node n     -> `O [ "node"    , H.to_json n ]

    let of_json = function
      | `O [ "contents", j ] -> `Contents (ContentsMeta.of_json j)
      | `O [ "node"    , j ] -> `Node (H.of_json j)
      | j -> Ezjsonm.parse_error j "Node.of_json"

    let write t buf = match t with
      | `Contents x -> ContentsMeta.write x (tag buf 0)
      | `Node x -> H.write x (tag buf 1)

    let size_of t = 1 + match t with
      | `Contents x -> ContentsMeta.size_of x
      | `Node x -> H.size_of x

    let read buf = match untag buf with
      | 0 -> `Contents (ContentsMeta.read buf)
      | 1 -> `Node (H.read buf)
      | n -> Tc.Reader.error "Vertex.read parse error (tag=%d)" n

  end

  include Tc.List (Tc.Pair (S)(X) )

end

module From_v0_11 (I: Irmin.S) = struct

  module H = Tc.String  (* hashes are strings *)
  module M = Tc.Unit    (* no metadata *)
  module S = Tc.String  (* steps are strings *)
  module R = Tc.String  (* references are strings *)
  module C = Tc.String  (* contents are strings *)

  module Commit = Commit_v0_11(H)
  module Tree = Tree_v0_11(H)(M)(S)

  module Tbl = Hashtbl.Make(H)

  type t = {
    repo    : I.repo;
    contents: I.Contents.Hash.t Tbl.t;
    tree    : I.Tree.Hash.t Tbl.t;
    commit  : I.Commit.Hash.t Tbl.t;
    ao      : (string * string) list ;
    rw      : (string * string) list;
  }

  let v repo ao rw =
    { repo;
      contents = Tbl.create 27;
      tree = Tbl.create 27;
      commit = Tbl.create 27;
      ao; rw }

  let pp_key ppf (k:H.t) =
    let `Hex x = Hex.of_string k in
    Fmt.string ppf x

  let rw_val h = H.read (Mstruct.of_string h)
  let ao_key h = I.Commit.Hash.(tts pp (of_raw (Cstruct.of_string h)))

  let find t k =
    try List.assoc (ao_key k) t
    with Not_found -> Fmt.failwith "Cannot find key %a in slice" pp_key k

  let find_contents t k =
    match I.Contents.of_string (find t.ao k) with
    | Ok x -> x
    | Error (`Msg e) -> Fmt.failwith "Cannot read contents %a: %s" pp_key k e

  let find_tree t k =
    let v = Mstruct.of_string (find t.ao k) in
    Tree.read v

  let find_commit t k =
    let v = Mstruct.of_string (find t.ao k) in
    Commit.read v

  let find_branch t r =
    try rw_val (List.assoc r t.rw)
    with Not_found ->Fmt.failwith "Cannot find branch %S in slice" r

  let branch x = match I.Branch.of_string x with
    | Ok x -> x
    | Error (`Msg e) -> Fmt.failwith "branch %S: %s" x e

  let step s = match I.Key.step_of_string s with
    | Ok x -> x
    | Error (`Msg e) -> Fmt.failwith "step %s: %s" s e

  let metadata () = I.Metadata.default

  let import_contents t (k:H.t): I.Contents.Hash.t Lwt.t =
    try Lwt.return (Tbl.find t.contents k)
    with Not_found ->
      let x = find_contents t k in
      I.Private.Contents.add (I.Private.Repo.contents_t t.repo) x >|= fun k' ->
      Tbl.add t.contents k k';
      k'

  let rec import_tree t (k:H.t): I.Tree.Hash.t Lwt.t =
    try Lwt.return (Tbl.find t.tree k)
    with Not_found ->
      let x = find_tree t k in
      Lwt_list.map_p (function
          | (s, `Contents (c, m)) ->
            import_contents t c >|= fun c ->
            step s, `Contents (c, metadata m)
          | (s, `Node n) ->
            import_tree t n >|= fun n ->
            step s, `Node n
        ) x
      >>= fun tree ->
      let tree = I.Private.Node.Val.v tree in
      I.Private.Node.add (I.Private.Repo.node_t t.repo) tree >|= fun k' ->
      Tbl.add t.tree k k';
      k'

  let rec import_commit t (k:H.t): I.Commit.Hash.t Lwt.t =
    try Lwt.return (Tbl.find t.commit k)
    with Not_found ->
      let x = find_commit t k in
      let info =
        Irmin.Info.v ~date:x.task.date ~author:x.task.owner
          (String.concat "\n" x.task.msgs)
      in
      Lwt_list.map_p (import_commit t) x.parents >>= fun parents ->
      import_tree t x.node >>= fun node ->
      let commit = I.Private.Commit.Val.v ~parents ~node ~info in
      I.Private.Commit.add (I.Private.Repo.commit_t t.repo) commit >|= fun k' ->
      Tbl.add t.commit k k';
      k'

  let import_branch t (r:R.t): I.Branch.t Lwt.t =
    let x = find_branch t r in
    let b = branch r in
    import_commit t x >>= fun h ->
    I.Private.Branch.set (I.Private.Repo.branch_t t.repo) b h >|= fun () ->
    b

  let import repo (module DB: DB) =
    let t = v repo DB.ao DB.rw in
    Lwt_list.map_p (fun (r, _) ->
        import_branch t r
      ) DB.rw
    >|= fun branches ->
    Fmt.pr "Successfuly imported the branches: %a\n%!"
      Fmt.(list ~sep:(unit " ") I.Branch.pp) branches

  let string_of_node x = tts Irmin.Type.(pp_json I.Private.Node.Val.t) x
  let string_of_commit x = tts Irmin.Type.(pp_json I.Private.Commit.Val.t) x

  let export_ao repo =
    I.Repo.export repo ~full:true >>= fun slice ->
    let ao = ref [] in
    let add x = ao := x :: !ao in
    I.Private.Slice.iter slice (function
        |`Node (h, n)     ->
          add (tts I.Tree.Hash.pp h, string_of_node n);
          Lwt.return ()
        |`Commit (h, c)   ->
          add (tts I.Commit.Hash.pp h, string_of_commit c);
          Lwt.return ()
        |`Contents (h, c) ->
          add (tts I.Contents.Hash.pp h, tts I.Contents.pp c);
          Lwt.return ()
      ) >|= fun () ->
    !ao

  let export_rw repo =
    I.Private.Branch.list (I.Private.Repo.branch_t repo) >>= fun branches ->
    Lwt_list.map_p (fun b ->
        I.Private.Branch.find (I.Private.Repo.branch_t repo) b >|= function
        | Some v -> b, v
        | None   -> assert false
      ) branches
    >|= fun rw ->
    List.map (fun (b, v) ->
        tts I.Branch.pp b, tts I.Commit.Hash.pp v
      ) rw

  let export repo =
    export_ao repo >>= fun ao ->
    export_rw repo >|= fun rw ->
    let module DB = struct
      let ao = ao
      let rw = rw
    end in
    (module DB: DB)
end
