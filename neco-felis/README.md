# neco-felis

This directory contains the Felis implementation of the neco compiler.

## Current Status

This directory is a workspace root.

`neco-felis-bin/` is the current bootstrap binary package.

`neco-felis-json/` contains the current JSON-oriented manifest parsing helpers
used by the bootstrap binary.

`neco-felis-parser/` contains the current Felis source parsing helpers used by
the bootstrap binary.

The current bootstrap binary reads a package root from its first command-line
argument, loads `neco-package.json`, opens the first
`felis-bin-entrypoints` source file through `neco-felis-json`, parses the Felis
source through `neco-felis-parser`, and writes `./a.out` in the current
directory.

The emitted `a.out` is a minimal Linux x86_64 ELF executable. The current
bootstrap path recognizes a single parsed statement shape at a time and emits
the corresponding Linux x86_64 syscall sequence for `exit(<small integer>)` or
`write(<string literal>)`.
