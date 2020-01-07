Irmin-IndexedDB
===============

Copyright Thomas Leonard, 2019

This is an Irmin backend that stores the data in the web-browser's IndexedDB store.


Instructions
------------

You'll need to pin a fixed version of `irmin-git` first:

    opam pin add -yn irmin-git.1.4.0 https://github.com/talex5/irmin.git#1.4.0-cuekeeper

To create an Irmin store, use e.g.

    module I = Irmin_IDB.Make(Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)
    let () =
      let config = Irmin_IDB.config "MyProg" in
      I.v config make_task >>= fun store ->
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
