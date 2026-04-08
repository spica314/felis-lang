# open-read-close

This example follows the Linux syscall shape:

- `IO::open path_bytes flags mode` returns a file descriptor.
- `IO::close fd` closes that file descriptor.

The program opens a packaged text file with `O_RDONLY` (`0`), reads from it,
closes the descriptor, and writes the bytes to stdout.
