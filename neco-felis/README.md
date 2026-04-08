# neco-felis

This directory contains the Felis implementation of the neco compiler.

## Current Status

This directory is a workspace root.

`neco-felis-bin/` is the current bootstrap binary package.

The current bootstrap binary writes `./a.out` in the current directory.

The emitted `a.out` is a minimal Linux x86_64 ELF executable whose only
behavior is `exit(0)` via syscall 60.
