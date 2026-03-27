# Test Cases

This directory contains source-level compiler test cases and package-shaped
compiler test fixtures.

## Purpose

- Place Felis programs here when they exist primarily to validate compiler
  behavior.
- Place small package directories here when a test case needs manifest-driven
  loading or multiple source files.
- Place small workspace-shaped directories here when a test case needs a root
  manifest that enumerates member packages and package-to-package
  dependencies.
- Organize cases by concern as the suite grows, such as parsing, type checking,
  or diagnostics.

## Naming Guidance

- Prefer file and directory names that describe the behavior under test.
- Keep reusable helper assets next to the cases that depend on them unless a
  broader shared layout becomes necessary.
