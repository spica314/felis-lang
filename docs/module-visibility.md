# Module Visibility

This document explains how Felis source code makes modules and imported symbols
part of a public module surface.

## Public Module Declarations

`#pub #mod` declares a child module and exposes that child module through the
current module's public surface.

This allows a module to define structure and export that structure at the same
time.

For example:

```felis
#pub #mod primitive;
```

## Public Re-Exports

`#pub #use` exposes a symbol through the current module's public surface.

This allows a module to import or bind a symbol locally and then make that
symbol visible to users of the module.

For example:

```felis
#bind_builtin "i32" #as i32;
#pub #use i32;
```

## Design Direction

Public module declarations and public re-exports provide explicit control over
which modules and symbols become part of a package's observable module surface.

They should remain explicit in source code so the exported API can be read from
module definitions without relying on implicit export rules.
