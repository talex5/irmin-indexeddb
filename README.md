Irmin-IndexedDB
===============

Copyright Thomas Leonard, 2015

This is an Irmin backend that stores the data in the web-browser's IndexedDB store.


Instructions
------------

You'll need a version of `bin_prot` that works with `js_of_ocaml` and doesn't depend on `Base`,
and a version of Irmin that doesn't depend on cohttp:

    opam pin add -yn bin_prot.114.06+90 'https://github.com/talex5/bin_prot.git#cuekeeper'
    opam pin add -yn irmin.0.10.1 https://github.com/talex5/irmin.git#cuekeeper

To create an Irmin store, use e.g.

    module I = Irmin.Basic(Irmin_IDB.Make)(Irmin.Contents.String)
    let () =
      let config = Irmin_IDB.config "MyProg" in
      I.create config make_task >>= fun store ->
      ...

The argument to `Irmin_IDB.config` is the name of the database to use (default "Irmin").

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


[mirage]: http://openmirage.org/
