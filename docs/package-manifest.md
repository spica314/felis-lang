# Package Manifest

This document explains the package manifest format used by Felis packages.

## Overview

Packages are described by a manifest file named `neco-package.json`.

The manifest is the package-level entry point for metadata that tools can read
without inspecting Felis source files directly.

## Required Fields

The current functional requirements cover these manifest fields:

- `name`: identifies the package.
- `felis-lib-entrypoint`: identifies the Felis library entrypoint file for the
  package.

For example:

```json
{
  "name": "std",
  "felis-lib-entrypoint": "src/lib.fe"
}
```

## Design Direction

The manifest should capture package-level information that must be understood by
tools before loading Felis modules.

Additional manifest fields may be defined later, but the functional
requirements currently constrain only the package name field and the Felis
library entrypoint field.
