# Package Manifest

This document explains the package and workspace manifest format used by Felis
tools.

## Overview

Packages and workspaces are described by a manifest file named
`neco-package.json`.

The manifest is the entry point for metadata that tools can read without
inspecting Felis source files directly.

A manifest is either:

- a package manifest, or
- a workspace manifest.

The two forms are distinct. A workspace root does not also act as a package in
the same `neco-package.json`.

## Package Fields

The current functional requirements cover these manifest fields:

- `name`: identifies the package.
- `felis-lib-entrypoint`: identifies the Felis library entrypoint file for the
  package.
- `dependencies`: identifies other packages required by the package.
- `felis-bin-entrypoints`: identifies one or more Felis binary entrypoint files
  for the package.

Binary entrypoint source files can then mark the selected function with an
`#entrypoint` declaration ending with `;`, for example
`#entrypoint main;`.

For example:

```json
{
  "name": "std_core",
  "felis-lib-entrypoint": "src/lib.fe"
}
```

Or, for a package that exposes binaries and depends on another workspace
package:

```json
{
  "name": "workspace-app",
  "dependencies": {
    "workspace-lib": {
      "workspace": true
    }
  },
  "felis-bin-entrypoints": ["src/main.fe"]
}
```

## Workspace Fields

A workspace manifest declares a workspace root and its member packages through:

- `workspace.members`: identifies one or more package directories relative to
  the workspace root.

For example:

```json
{
  "workspace": {
    "members": ["workspace-app", "workspace-lib"]
  }
}
```

## Constraints

The current workspace design intentionally stays narrow:

- A manifest cannot combine package fields such as `name` with a `workspace`
  field.
- `workspace.members` entries are relative paths from the workspace root.
- Each workspace member directory must contain its own `neco-package.json`.
- Workspace member package names must be unique within the workspace.
- A package can declare a dependency on another workspace member through:

```json
{
  "dependencies": {
    "workspace-lib": {
      "workspace": true
    }
  }
}
```

The fixture at `tests/testcases/workspace-basic/` captures this shape.

## Design Direction

The manifest should capture package-level and workspace-level information that
must be understood by tools before loading Felis modules.

Additional manifest fields may be defined later, but the current functional
requirements constrain only:

- package name and Felis entrypoint fields,
- workspace member enumeration, and
- package dependencies on workspace members.
