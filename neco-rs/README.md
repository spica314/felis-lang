# neco-rs

This directory contains the Rust crates for the bootstrap implementation of the
neco compiler. The Cargo workspace lives at the repository root.

## Current Status

The Rust bootstrap implementation is active but intentionally narrow. It can
parse Felis packages and workspaces, lower a supported subset of Felis into a
small internal operation model, and emit Linux x86-64 ELF executables for the
runtime fixtures covered by the test suite.

The implementation is not a complete Felis compiler yet. Many language
features are still handled by targeted lowering rules for entrypoint-oriented
programs, built-in primitives, and the current standard-library surface.

## Crates

- `neco-rs` is the compiler driver, lowering pipeline, Linux x86-64 code
  generator, CLI entrypoint, and runtime fixture tests.
- `neco-rs-parser` parses package manifests and Felis source syntax.
- `neco-rs-json` is the small JSON parser used for Felis package manifests.
- `neco-rs-elf` builds the ELF executable container used by the bootstrap
  backend.
