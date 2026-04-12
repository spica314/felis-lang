# open-array-path

This example exercises `IO::open` with a runtime-built `u8` array path.

The program writes `message.txt\0` into a `u8` array, opens that path with
`O_RDONLY` (`0`), reads the file, closes the descriptor, and writes the bytes
to stdout.
