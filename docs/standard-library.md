# Standard Library Surface

This document records the currently documented public surface of the Felis
standard library.

## Workspace Layout

The standard library is now split into two workspace packages:

- [`std/std_core/`](../std/std_core/)
- [`std/std_math/`](../std/std_math/)

`std_core` exposes `primitive`, `io`, `path`, `string`, `option`, and `result`.

`std_math` exposes `math`.

## Primitive Module

The `primitive` branch exposes compiler-provided builtin bindings as ordinary
source-level symbols.

[`std/std_core/src/primitive/array.fe`](../std/std_core/src/primitive/array.fe)
currently re-exports:

- `Array`
- `ArrayVL`
- `ArrayVLPTX`
- `array_get`
- `array_set`
- `array_len`
- `append_u8`
- `append_nul_terminated_u8`
- `append_dyn_nul_terminated_u8`
- `fill_u8`

[`std/std_core/src/primitive/vec.fe`](../std/std_core/src/primitive/vec.fe)
currently re-exports:

- `Vec`
- `vec_arrayvl`
- `vec_capacity`
- `vec_len`
- `vec_get`
- `vec_set`
- `vec_push`

[`std/std_core/src/primitive/bool.fe`](../std/std_core/src/primitive/bool.fe)
currently re-exports:

- `bool`
- `true`
- `false`
- `bool_and`
- `bool_or`
- `bool_not`

[`std/std_core/src/primitive/f32.fe`](../std/std_core/src/primitive/f32.fe)
currently re-exports:

- `f32`
- `f32_add`
- `f32_sub`
- `f32_mul`
- `f32_div`
- `f32_sqrt`
- `f32_eq`
- `f32_lte`
- `f32_lt`
- `f32_gte`
- `f32_gt`
- `f32_from_i32`
- `f32_from_i64`
- `f32_from_u8`

[`std/std_core/src/primitive/i32.fe`](../std/std_core/src/primitive/i32.fe)
currently re-exports:

- `i32`
- `i32_add`
- `i32_sub`
- `i32_mul`
- `i32_div`
- `i32_mod`
- `i32_xor`
- `i32_shl`
- `i32_shr`
- `i32_eq`
- `i32_lte`
- `i32_lt`
- `i32_gte`
- `i32_gt`
- `i32_from_u8`
- `i32_from_i64`
- `i32_from_f32`

[`std/std_core/src/primitive/i64.fe`](../std/std_core/src/primitive/i64.fe)
currently re-exports:

- `i64`
- `i64_add`
- `i64_sub`
- `i64_mul`
- `i64_div`
- `i64_mod`
- `i64_eq`
- `i64_lte`
- `i64_lt`
- `i64_gte`
- `i64_gt`
- `i64_from_i32`
- `i64_from_u8`
- `i64_from_f32`

[`std/std_core/src/primitive/reference.fe`](../std/std_core/src/primitive/reference.fe)
currently re-exports:

- `ref_get`
- `ref_set`

[`std/std_core/src/primitive/u8.fe`](../std/std_core/src/primitive/u8.fe)
currently re-exports:

- `u8`
- `u8_add`
- `u8_sub`
- `u8_mul`
- `u8_div`
- `u8_mod`
- `u8_eq`
- `u8_lte`
- `u8_lt`
- `u8_gte`
- `u8_gt`
- `u8_from_i32`
- `u8_from_i64`
- `u8_from_f32`

## IO Module

The `io` branch exposes the builtin `IO` effect as an ordinary public symbol.

[`std/std_core/src/io.fe`](../std/std_core/src/io.fe) currently re-exports:

- `IO`
- `FileDescriptor`

The `hello-world` testcase shows this binding being imported as
`std_core::io::IO` and used in a function declaration with `#with IO`. In this
context, `#with` attaches an Algebraic Effects `Effect` to the function
declaration.

## PTX Module

[`std/std_core/src/ptx.fe`](../std/std_core/src/ptx.fe) exposes the PTX effect
and special-register helpers used by GPU fixtures:

- `PTX`
- `ctaid_x`
- `ctaid_y`
- `ctaid_z`
- `ntid_x`
- `ntid_y`
- `ntid_z`
- `tid_x`
- `tid_y`
- `tid_z`

## Path Module

[`std/std_core/src/path.fe`](../std/std_core/src/path.fe) exposes the builtin
`PathBuf` type and helper functions for NUL-terminated path buffers:

- `PathBuf`
- `pathbuf_new`
- `pathbuf_push`
- `pathbuf_pop`

## Option And Result

[`std/std_core/src/option.fe`](../std/std_core/src/option.fe) defines a public
generic `Option` type with the public constructors:

- `some`
- `none`

[`std/std_core/src/result.fe`](../std/std_core/src/result.fe) defines a public
generic `Result` type with the public constructors:

- `ok`
- `err`

## Mathematics Modules

The `math` branch currently exposes `nat` and `eq`.

### Natural Numbers

[`std/std_math/src/math/nat/nat.fe`](../std/std_math/src/math/nat/nat.fe)
defines a public type
`Nat` with the public constructors:

- `zero`
- `succ`

[`std/std_math/src/math/nat/nat_add.fe`](../std/std_math/src/math/nat/nat_add.fe)
defines:

- `nat_add`
- `nat_add_zero_x_eq_x`
- `nat_add_x_zero_eq_x`

These declarations show that the standard library surface is not limited to
primitive bindings. It can also expose ordinary functions and proof-oriented
artifacts as public symbols.

The declaration forms used for these exports are summarized in
[`docs/declaration-forms.md`](./declaration-forms.md).

### Equality

[`std/std_math/src/math/eq/eq.fe`](../std/std_math/src/math/eq/eq.fe) defines
a public proposition `Eq[u]` with the public constructor:

- `eq_refl`

## Package-Qualified References Inside `std`

Standard library modules may reference each other through package-qualified
paths rooted at `#package::`.

For example,
[`std/std_math/src/math/nat/nat_add.fe`](../std/std_math/src/math/nat/nat_add.fe)
imports `Eq` and `Nat` through fully qualified package paths before re-exported
definitions depend on them.
