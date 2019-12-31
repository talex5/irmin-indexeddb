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
