Irmin-IndexedDB
===============

Copyright Thomas Leonard, 2020

This is an Irmin backend that stores the data in the web-browser's IndexedDB store.


Instructions
------------

You can create stores using either the standard Git format, or using Irmin's own format.
For Git format (you'll need to add `irmin-git` as a dependency), use:

```ocaml
(* A Git-format store. This data can be exported and used with the regular Git
   tools. It can also read data produced by older versions of irmin-indexeddb. *)
module I = Irmin_git.Generic(Irmin_indexeddb.Content_store_git)(Irmin_indexeddb.Branch_store)
    (Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)
```

For Irmin format, use:

```ocaml
(* An Irmin-format store. This allows storing custom metadata or using
   different hash functions, but is not compatible with the Git tools or with
   databases created by older versions of irmin-indexeddb. *)
module I = Irmin.Make(Irmin_indexeddb.Content_store_non_git)(Irmin_indexeddb.Branch_store)
    (Irmin.Metadata.None)(Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)(Irmin.Hash.SHA256)
```

To create a store, use e.g.

    let () =
      let config = Irmin_indexeddb.config "MyProg" in
      I.v config make_task >>= fun store ->
      ...

The argument to `Irmin_indexeddb.config` is the name of the database to use.

Note: In order to provide notifications (to instances running in other tabs),
the backend will also write the current branch head hash into HTML local
storage.


Bugs
----

Please any send questions or comments to the mirage mailing list:

http://lists.xenproject.org/cgi-bin/mailman/listinfo/mirageos-devel

Bugs can be reported on the mailing list or as GitHub issues:

https://github.com/talex5/irmin-indexeddb/issues


Conditions
----------

See [LICENSE.md](LICENSE.md).


[mirage]: https://mirage.io
