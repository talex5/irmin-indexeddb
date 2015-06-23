Irmin-IndexedDB
===============

Copyright Thomas Leonard, 2015

This is an Irmin backend that stores the data in the web-browser's IndexedDB store.


Instructions
------------

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

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
USA


Full details can be found in the LICENSE file.


[mirage]: http://openmirage.org/
