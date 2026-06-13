# Appendix: Builtin Symbols

Builtin symbols are compiler-provided values that the standard library exposes with `#bind_builtin`.
Import them through `std_core` modules instead of relying on implicit names.

## Types, Values, and Effects

| Symbol | Type | Description |
| --- | --- | --- |
| `i32` | `Type[0]` | Signed 32-bit integer type used by integer literals with the `i32` suffix. |
| `i64` | `Type[0]` | Signed 64-bit integer type used by integer literals with the `i64` suffix. |
| `f32` | `Type[0]` | 32-bit floating-point type used by decimal literals with the `f32` suffix. |
| `u8` | `Type[0]` | Unsigned 8-bit integer type used by byte values and character literals. |
| `bool` | `Type[0]` | Boolean type used by `#if` and comparison results. |
| `true` | `bool` | Boolean true value. |
| `false` | `bool` | Boolean false value. |
| `Array` | `(t : Type[0]) -> (len : i32) -> Type[0]` | Fixed-size array type constructor; the length must be a constant `i32` literal. |
| `ArrayVL` | `(t : Type[0]) -> Type[0]` | Runtime-sized array value type constructor. |
| `ArrayVLPTX` | `(t : Type[0]) -> Type[0]` | PTX device-side runtime-sized array value type constructor. |
| `PTX` | effect | Effect marker used by functions that compile to PTX kernels. |
| `IO` | effect | Effect marker used by functions that perform builtin IO operations. |
| `FileDescriptor` | `Type[0]` | File descriptor value type returned by IO descriptor operations. |
| `PathBuf` | `Type[0]` | Mutable NUL-terminated file path buffer. |

## `i32` Functions

| Symbol | Description |
| --- | --- |
| `i32_add` | `(lhs : i32) -> (rhs : i32) -> i32`; returns `lhs + rhs`. |
| `i32_sub` | `(lhs : i32) -> (rhs : i32) -> i32`; returns `lhs - rhs`. |
| `i32_mul` | `(lhs : i32) -> (rhs : i32) -> i32`; returns `lhs * rhs`. |
| `i32_div` | `(lhs : i32) -> (rhs : i32) -> i32`; returns integer division of `lhs` by `rhs`. |
| `i32_mod` | `(lhs : i32) -> (rhs : i32) -> i32`; returns the remainder of `lhs / rhs`. |
| `i32_xor` | `(lhs : i32) -> (rhs : i32) -> i32`; returns bitwise exclusive-or. |
| `i32_shl` | `(lhs : i32) -> (rhs : i32) -> i32`; shifts `lhs` left by `rhs` bits. |
| `i32_shr` | `(lhs : i32) -> (rhs : i32) -> i32`; shifts `lhs` right by `rhs` bits. |
| `i32_eq` | `(lhs : i32) -> (rhs : i32) -> bool`; returns whether the two values are equal. |
| `i32_lte` | `(lhs : i32) -> (rhs : i32) -> bool`; returns whether `lhs <= rhs`. |
| `i32_lt` | `(lhs : i32) -> (rhs : i32) -> bool`; returns whether `lhs < rhs`. |
| `i32_gte` | `(lhs : i32) -> (rhs : i32) -> bool`; returns whether `lhs >= rhs`. |
| `i32_gt` | `(lhs : i32) -> (rhs : i32) -> bool`; returns whether `lhs > rhs`. |
| `i32_from_u8` | `(value : u8) -> i32`; converts a `u8` value to `i32`. |
| `i32_from_i64` | `(value : i64) -> i32`; converts an `i64` value to `i32`. |
| `i32_from_f32` | `(value : f32) -> i32`; converts an `f32` value to `i32`. |

## `i64` Functions

| Symbol | Description |
| --- | --- |
| `i64_add` | `(lhs : i64) -> (rhs : i64) -> i64`; returns `lhs + rhs`. |
| `i64_sub` | `(lhs : i64) -> (rhs : i64) -> i64`; returns `lhs - rhs`. |
| `i64_mul` | `(lhs : i64) -> (rhs : i64) -> i64`; returns `lhs * rhs`. |
| `i64_div` | `(lhs : i64) -> (rhs : i64) -> i64`; returns integer division of `lhs` by `rhs`. |
| `i64_mod` | `(lhs : i64) -> (rhs : i64) -> i64`; returns the remainder of `lhs / rhs`. |
| `i64_eq` | `(lhs : i64) -> (rhs : i64) -> bool`; returns whether the two values are equal. |
| `i64_lte` | `(lhs : i64) -> (rhs : i64) -> bool`; returns whether `lhs <= rhs`. |
| `i64_lt` | `(lhs : i64) -> (rhs : i64) -> bool`; returns whether `lhs < rhs`. |
| `i64_gte` | `(lhs : i64) -> (rhs : i64) -> bool`; returns whether `lhs >= rhs`. |
| `i64_gt` | `(lhs : i64) -> (rhs : i64) -> bool`; returns whether `lhs > rhs`. |
| `i64_from_i32` | `(value : i32) -> i64`; converts an `i32` value to `i64`. |
| `i64_from_u8` | `(value : u8) -> i64`; converts a `u8` value to `i64`. |
| `i64_from_f32` | `(value : f32) -> i64`; converts an `f32` value to `i64`. |

## `f32` Functions

| Symbol | Description |
| --- | --- |
| `f32_add` | `(lhs : f32) -> (rhs : f32) -> f32`; returns `lhs + rhs`. |
| `f32_sub` | `(lhs : f32) -> (rhs : f32) -> f32`; returns `lhs - rhs`. |
| `f32_mul` | `(lhs : f32) -> (rhs : f32) -> f32`; returns `lhs * rhs`. |
| `f32_div` | `(lhs : f32) -> (rhs : f32) -> f32`; returns `lhs / rhs`. |
| `f32_sqrt` | `(value : f32) -> f32`; returns the square root of `value`. |
| `f32_eq` | `(lhs : f32) -> (rhs : f32) -> bool`; returns whether the two values are equal. |
| `f32_lte` | `(lhs : f32) -> (rhs : f32) -> bool`; returns whether `lhs <= rhs`. |
| `f32_lt` | `(lhs : f32) -> (rhs : f32) -> bool`; returns whether `lhs < rhs`. |
| `f32_gte` | `(lhs : f32) -> (rhs : f32) -> bool`; returns whether `lhs >= rhs`. |
| `f32_gt` | `(lhs : f32) -> (rhs : f32) -> bool`; returns whether `lhs > rhs`. |
| `f32_from_i32` | `(value : i32) -> f32`; converts an `i32` value to `f32`. |
| `f32_from_i64` | `(value : i64) -> f32`; converts an `i64` value to `f32`. |
| `f32_from_u8` | `(value : u8) -> f32`; converts a `u8` value to `f32`. |

## `u8` Functions

| Symbol | Description |
| --- | --- |
| `u8_add` | `(lhs : u8) -> (rhs : u8) -> u8`; returns `lhs + rhs`. |
| `u8_sub` | `(lhs : u8) -> (rhs : u8) -> u8`; returns `lhs - rhs`. |
| `u8_mul` | `(lhs : u8) -> (rhs : u8) -> u8`; returns `lhs * rhs`. |
| `u8_div` | `(lhs : u8) -> (rhs : u8) -> u8`; returns integer division of `lhs` by `rhs`. |
| `u8_mod` | `(lhs : u8) -> (rhs : u8) -> u8`; returns the remainder of `lhs / rhs`. |
| `u8_eq` | `(lhs : u8) -> (rhs : u8) -> bool`; returns whether the two values are equal. |
| `u8_lte` | `(lhs : u8) -> (rhs : u8) -> bool`; returns whether `lhs <= rhs`. |
| `u8_lt` | `(lhs : u8) -> (rhs : u8) -> bool`; returns whether `lhs < rhs`. |
| `u8_gte` | `(lhs : u8) -> (rhs : u8) -> bool`; returns whether `lhs >= rhs`. |
| `u8_gt` | `(lhs : u8) -> (rhs : u8) -> bool`; returns whether `lhs > rhs`. |
| `u8_from_i32` | `(value : i32) -> u8`; converts an `i32` value to `u8`. |
| `u8_from_i64` | `(value : i64) -> u8`; converts an `i64` value to `u8`. |
| `u8_from_f32` | `(value : f32) -> u8`; converts an `f32` value to `u8`. |

## `bool` Functions

| Symbol | Description |
| --- | --- |
| `bool_and` | `(lhs : bool) -> (rhs : bool) -> bool`; returns logical conjunction. |
| `bool_or` | `(lhs : bool) -> (rhs : bool) -> bool`; returns logical disjunction. |
| `bool_not` | `(value : bool) -> bool`; returns logical negation. |

## Array Operations

These operations require a function annotated with `#with IO`.

| Symbol | Description |
| --- | --- |
| `array_get` | `(array : Array t n or ArrayVL t) -> (index : i32 or i64) -> t #with IO`; reads one element. |
| `array_set` | `(array : &^ Array t n or &^ ArrayVL t) -> (index : i32 or i64) -> (value : t) -> () #with IO`; writes one element. |
| `array_len` | `(array : ArrayVL t) -> i32 #with IO`; returns the runtime length of an `ArrayVL t`. |

## PTX Special Registers

These functions require a function annotated with `#with PTX`.

| Symbol | Description |
| --- | --- |
| `ctaid_x` | `() -> i32 #with PTX`; returns the CTA/block index in the x dimension. |
| `ctaid_y` | `() -> i32 #with PTX`; returns the CTA/block index in the y dimension. |
| `ctaid_z` | `() -> i32 #with PTX`; returns the CTA/block index in the z dimension. |
| `ntid_x` | `() -> i32 #with PTX`; returns the block dimension in the x dimension. |
| `ntid_y` | `() -> i32 #with PTX`; returns the block dimension in the y dimension. |
| `ntid_z` | `() -> i32 #with PTX`; returns the block dimension in the z dimension. |
| `tid_x` | `() -> i32 #with PTX`; returns the thread index in the x dimension. |
| `tid_y` | `() -> i32 #with PTX`; returns the thread index in the y dimension. |
| `tid_z` | `() -> i32 #with PTX`; returns the thread index in the z dimension. |

## Reference Functions

These operations require a function annotated with `#with IO`.

| Symbol | Description |
| --- | --- |
| `ref_get` | `(t : Type[0]) -> (r : & t) -> t #with IO`; reads through a reference. |
| `ref_set` | `(t : Type[0]) -> (r : &^ t) -> (x : t) -> () #with IO`; writes through an exclusive reference. |

## `IO` Operations

All `IO` operations must be used in a function annotated with `#with IO`.

| Symbol | Description |
| --- | --- |
| `IO::stdin` | `FileDescriptor #with IO`; returns the standard input descriptor. |
| `IO::stdout` | `FileDescriptor #with IO`; returns the standard output descriptor. |
| `IO::sys_read` | `(fd : FileDescriptor) -> (buffer : Array u8 n or ArrayVL u8) -> (len : i32) -> i32 #with IO`; reads bytes and returns the byte count. |
| `IO::sys_write` | `(fd : FileDescriptor) -> (bytes : Array u8 n or ArrayVL u8) -> (len : i32) -> () #with IO`; writes bytes to a descriptor. |
| `IO::panic` | `(message : & ArrayVL u8) -> () #with IO`; writes the message to standard error and terminates with exit code 101. |
| `IO::sys_open` | `(path : & PathBuf) -> (flags : i32) -> (mode : i32) -> FileDescriptor #with IO`; opens a path and returns a descriptor. |
| `IO::sys_close` | `(fd : FileDescriptor) -> () #with IO`; closes a descriptor. |
| `IO::sys_exit` | `(code : i32 or i64 or u8) -> () #with IO`; terminates the process with the given exit code. |
| `IO::array_new` | `(t : Type[0]) -> (len : i32) -> Array t len #with IO`; allocates a fixed-size array. |
| `IO::arrayvl_new` | `(t : Type[0]) -> (len : i32) -> ArrayVL t #with IO`; allocates a runtime-sized array value. |
| `IO::arrayvl_replace` | `(t : Type[0]) -> (dest : ArrayVL t) -> (source : ArrayVL t) -> () #with IO`; replaces the backing storage of a dynamic array. |
| `IO::arrayvlptx_new` | `(t : Type[0]) -> (len : i32) -> ArrayVLPTX t #with IO`; allocates a device-side runtime-sized array value. |
| `IO::arrayvl_to_ptx` | `(t : Type[0]) -> (host : ArrayVL t) -> (device : ArrayVLPTX t) -> () #with IO`; copies a host dynamic array to device storage. |
| `IO::arrayvl_from_ptx` | `(t : Type[0]) -> (device : ArrayVLPTX t) -> (host : ArrayVL t) -> () #with IO`; copies a device dynamic array to host storage. |
| `IO::pathbuf_new` | `(capacity : i32) -> PathBuf #with IO`; allocates an empty NUL-terminated path buffer. |
| `IO::pathbuf_push` | `(path : &^ PathBuf) -> (source : & ArrayVL u8) -> () #with IO`; appends NUL-terminated bytes while preserving the trailing NUL. |
| `IO::pathbuf_pop` | `(path : &^ PathBuf) -> () #with IO`; removes the last path component while preserving the trailing NUL. |
| `IO::arg` | `(index : i32) -> & ArrayVL u8 #with IO`; returns a command-line argument as a byte array reference. |
| `IO::cu_init` | `(flags : i32) -> i32 #with IO`; initializes the CUDA driver. |
| `IO::cu_device_get` | `(ordinal : i32) -> i32 #with IO`; returns a CUDA device handle. |
| `IO::cu_ctx_create_v2` | `(flags : i32) -> (device : i32) -> i32 #with IO`; creates a CUDA context. |
| `IO::cu_module_load_data` | `(ptx : & ArrayVL u8) -> i64 #with IO`; loads a CUDA module from PTX bytes. |
| `IO::cu_module_get_function` | `(module : i64) -> (name : & ArrayVL u8) -> i64 #with IO`; resolves a CUDA kernel function. |
| `IO::cu_launch_kernel` | CUDA kernel launch operation using a function handle, grid/block dimensions, and kernel arguments. |
