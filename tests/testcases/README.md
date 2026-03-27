# Test Cases

This directory contains source-level compiler test cases.

## Purpose

- Place Felis programs here when they exist primarily to validate compiler
  behavior.
- Organize cases by concern as the suite grows, such as parsing, type checking,
  or diagnostics.

## Naming Guidance

- Prefer file and directory names that describe the behavior under test.
- Keep reusable helper assets next to the cases that depend on them unless a
  broader shared layout becomes necessary.
