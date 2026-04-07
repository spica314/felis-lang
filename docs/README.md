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
- Declaration forms used by the standard library are documented in
  [`docs/declaration-forms.md`](./declaration-forms.md).
- Module visibility and public re-exports are documented in
  [`docs/module-visibility.md`](./module-visibility.md).
- The documented standard-library surface is summarized in
  [`docs/standard-library.md`](./standard-library.md).
- Package manifest structure is documented in
  [`docs/package-manifest.md`](./package-manifest.md).

## Repository Conventions

- Compiler implementation and bootstrap in this repository follow a strict
  dependency policy:
  building neco should not require anything beyond a Rust compiler, and the
  repository should not depend on external Rust crates or other non-Rust build
  tooling.
  This policy exists to keep behavior under project control as much as
  possible, to reduce supply-chain and security risk, and to preserve a
  realistic discipline for the self-hosted Felis compiler, where external
  dependencies cannot be assumed.
- [`std/`](../std/) is reserved for the Felis standard library.
- [`tests/`](../tests/) is reserved for compiler test cases.
- Documentation about how `std/` is organized should live in
  [`std/README.md`](../std/README.md) and in this `docs/` directory when broader
  repository context is needed.
- Documentation about how tests are organized should live in
  [`tests/README.md`](../tests/README.md) when repository-level guidance is
  needed.
