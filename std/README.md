# Standard Library

This directory is reserved for the Felis standard library.

## Purpose

- Place source files that define the standard library under `std/`.
- Keep compiler tests, design notes, and requirement documents outside this
  directory.

## Directory Policy

- `std/` is the canonical top-level location for standard library code.
- Subdirectories may be added here as the standard library grows.
- Documentation about language or repository design should remain in `docs/`.
- Compiler test cases should remain in `tests/`.

## Current Layout

- `neco-package.json` is a workspace root manifest.
- `std_core/` contains primitive builtin bindings and the `IO` effect package.
- `std_math/` contains mathematics-oriented modules such as natural numbers
  and equality.
