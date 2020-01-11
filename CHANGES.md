## v2.0

Update to Irmin 2.0 API. This way that stores are constructed has changed in Irmin 2.0.
Now, irmin-indexeddb just provides the raw contents and branch stores and the application
uses these to create a full Irmin store. This means that you can use irmin-indexeddb to
create either Git-format or Irmin-format stores, and you can avoid a dependency on irmin-git
if you don't need Git-format stores.

For Irmin format, use e.g.:

```ocaml
module I = Irmin.Make(Irmin_indexeddb.Content_store)(Irmin_indexeddb.Branch_store)
    (Irmin.Metadata.None)(Irmin.Contents.String)
    (Irmin.Path.String_list)(Irmin.Branch.String)(Irmin.Hash.SHA256)
```

For a Git format store, use:

```ocaml
module I = Irmin_git.Generic(Irmin_indexeddb.Content_store)(Irmin_indexeddb.Branch_store)
    (Irmin.Contents.String)(Irmin.Path.String_list)(Irmin.Branch.String)
```

If you have an existing database created by an older version of
irmin-indexeddb, then you *must* create a Git-format store.

## v1.3

Update to Irmin 1.3 API.

## v1.0

This version updates irmin-indexeddb to use the Irmin 1.0 API. This is a major
breaking change to the API because the Irmin API changed a lot in 1.0. When
upgrading from irmin-indexeddb 0.6, existing databases can still be read. To
upgrade from earlier releases you must first upgrade to v0.6 and let it convert
the data from the Irmin 0.10 format to Git format.

Attempting to open an old-format database using this version will raise the
exception `Format_too_old Irmin_0_10`. If your application used to use versions
of irmin-indexeddb before v0.6 then you should catch this and tell users to
first upgrade to a version of your application that uses v0.6.

Note that it is no longer possible to pass a custom hash to `Make`. `Irmin_git`
now always uses SHA1, for compatibilty with Git. It is not possible to upgrade
from a database using a different hash (I'm not aware of anyone using a different
hash).

Due to some missing type equalities in `irmin_git`, you will need to pin this
branch too:

    opam pin add irmin-git.1.0.0 https://github.com/talex5/irmin.git#1.0.0-cuekeeper

## v0.6

Data is now stored using the standard Git format, rather than the Irmin 0.10
custom format. This means that it is possible to export the data and read it
with Git and also that it is now possible to move to newer versions of Irmin
without losing data.

This version will convert any existing data to Git format, translating the old
`ao`/`rw` stores to new-format `ao_git`/`rw_git` stores. On my machine, converting
my CueKeeper history from 2015-03-09 to now (15,193 commits) took about 20
minutes in Firefox. You can provide a `log` argument to the new `create_full`
function to provide progress updates to users during the migration.

If the migration is interrupted (e.g. by closing the browser tab) then it will
restart from the beginning next time. The old-format stores remain after the
migration, in case something goes wrong.

The migration updates the schema version from 2 to 3, so that old versions of
this library will refuse to access it (so the old stores become effectively
read-only). The next version of this library will upgrade the version from 3 to
4 and delete the old-format stores. Newer versions of this library will refuse
to touch stores in the old format 2.

It is not easy to install this version of irmin-indexeddb because it requires
various pinned libraries. See the `.travis.yml` for the exact versions required.

## v0.5

Note: this release still uses Irmin 0.10.x, which is very out-of-date now.
However, this is a first step towards fixing the bit-rot.

- Update `js_of_ocaml` 2 -> 3.
- Update `base64` 2 -> 3.
- Upgrade opam 1 -> 2.
- Convert from oasis to dune.

## v0.4

Release never completed.
Was intended to support the now-obsolete Irmin 0.11.0 format.

## v0.3

- Updated for Irmin 0.10.0.
