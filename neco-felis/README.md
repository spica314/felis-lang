# neco-felis

This directory contains the Felis implementation of the neco compiler.

## Current Status

This directory is a workspace root.

`neco-felis-bin/` is the current bootstrap binary package.

The current bootstrap binary reads the compile target from its first command-line
argument, extracts the `IO::exit ...i32` literal in a minimal way, and writes
`./a.out` in the current directory.

The emitted `a.out` is a minimal Linux x86_64 ELF executable whose only
behavior is `exit(42)` for that fixture.
