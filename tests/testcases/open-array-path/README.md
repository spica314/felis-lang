# open-array-path

This example exercises `IO::open` with a `PathBuf` populated from a runtime-built
`u8` array path.

The program writes `message.txt\0` into a `u8` array, pushes those bytes into a
`PathBuf`, opens that path with `O_RDONLY` (`0`), reads the file, closes the
descriptor, and writes the bytes to stdout.
