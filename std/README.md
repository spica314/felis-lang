# Standard Library

This directory is reserved for the Felis standard library.

## Purpose

- Place source files that define the standard library under `std/`.
- Keep compiler sources, design notes, and requirement documents outside this
  directory.

## Directory Policy

- `std/` is the canonical top-level location for standard library code.
- Subdirectories may be added here as the standard library grows.
- Documentation about language or repository design should remain in `docs/`.

## Current Layout

- `src/lib.fe` exposes the package root surface.
- `src/primitive.fe` and `src/primitive/` contain primitive builtin bindings.
- `src/math.fe` and `src/math/` contain mathematics-oriented modules such as
  natural numbers and equality.
