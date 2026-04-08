# open-write-close

This example follows the Linux syscall shape:

- `IO::open path_bytes flags mode` returns a file descriptor.
- `IO::close fd` closes that file descriptor.

The program opens `created.txt` with `O_WRONLY | O_CREAT | O_TRUNC`,
writes a static byte string, and closes the descriptor.
