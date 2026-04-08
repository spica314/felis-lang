# neco-felis

This directory contains the Felis implementation of the neco compiler.

## Current Status

This directory is a workspace root.

`neco-felis-bin/` is the current bootstrap binary package.

The current bootstrap binary reads
`tests/testcases/exit-42/src/exit-42.fe`, extracts the `IO::exit ...i32`
literal in a minimal way, and writes `./a.out` in the current directory.

The emitted `a.out` is a minimal Linux x86_64 ELF executable whose only
behavior is `exit(42)` for that fixture.
