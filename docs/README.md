# Documentation

This directory contains project documentation.

## Functional Requirements

Primary functional requirements are managed in [`frd.json`](../frd.json).

[`frd.json`](../frd.json) is intended to be machine-processable with `jq` and
stable enough to serve as a reliable reference for both humans and AI-assisted
workflows.

- Specification: [`docs/frd.md`](./frd.md)
- Source of truth: [`frd.json`](../frd.json)

## Design Notes

- Language design rationale: [`docs/language-design.md`](./language-design.md)
- Builtin types and functions are documented as explicit source-level bindings
  in [`docs/language-design.md`](./language-design.md).
