# Standard Library Surface

This document records the currently documented public surface of the Felis
standard library.

## Workspace Layout

The standard library is now split into two workspace packages:

- [`std/std_core/`](../std/std_core/)
- [`std/std_math/`](../std/std_math/)

`std_core` exposes `primitive` and `io`.

`std_math` exposes `math`.

## Primitive Module

The `primitive` branch exposes compiler-provided builtin bindings as ordinary
source-level symbols.

[`std/std_core/src/primitive/array.fe`](../std/std_core/src/primitive/array.fe)
currently re-exports:

- `Array`

[`std/std_core/src/primitive/i32.fe`](../std/std_core/src/primitive/i32.fe)
currently re-exports:

- `i32`
- `i32_add`
- `i32_sub`
- `i32_mul`
- `i32_div`
- `i32_mod`

[`std/std_core/src/primitive/u8.fe`](../std/std_core/src/primitive/u8.fe)
currently re-exports:

- `u8`
- `u8_add`
- `u8_sub`
- `u8_mul`
- `u8_div`
- `u8_mod`

## IO Module

The `io` branch exposes the builtin `IO` effect as an ordinary public symbol.

[`std/std_core/src/io.fe`](../std/std_core/src/io.fe) currently re-exports:

- `IO`

The `hello-world` testcase shows this binding being imported as
`std_core::io::IO` and used in a function declaration with `#with IO`. In this
context, `#with` attaches an Algebraic Effects `Effect` to the function
declaration.

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
