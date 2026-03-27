# Tests

This directory is reserved for compiler-facing test cases.

## Purpose

- Place compiler validation inputs under `tests/`.
- Keep production-oriented Felis code such as the standard library under
  [`std/`](../std/).

## Directory Policy

- `tests/` is the canonical top-level location for compiler tests.
- Source-level test programs and small package-shaped test fixtures should live
  under [`tests/testcases/`](./testcases/).

## Current Layout

- `testcases/` contains compiler test inputs, including Felis source files and
  package-shaped fixtures.
