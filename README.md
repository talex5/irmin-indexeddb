Irmin-IndexedDB
===============

Copyright Thomas Leonard, 2015

This is an Irmin backend that stores the data in the web-browser's IndexedDB store.


Instructions
------------

You'll need a version of `bin_prot` that works with `js_of_ocaml` and doesn't depend on `Base`:

    opam pin add bin_prot.114.06+90 'https://github.com/talex5/bin_prot.git#cuekeeper'

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

Copyright (c) 2015 Thomas Leonard <talex5@gmail.com>

Permission to use, copy, modify, and distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


[mirage]: http://openmirage.org/
