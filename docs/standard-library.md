# Standard Library Surface

This document records the currently documented public surface of the Felis
standard library.

Detailed functional requirements remain in [`frd.json`](../frd.json). This file
summarizes the observable API shape exposed from [`std/`](../std/).

## Root Modules

The standard library root surface is declared in
[`std/src/lib.fe`](../std/src/lib.fe).

It currently exposes:

- `primitive` unconditionally.
- `io` unconditionally.
- `math` only when the `bootstrap` feature is not enabled.

This conditional export is expressed with `#[cfg(not(feature="bootstrap"))]`
on the public `math` module declaration. The `cfg` attribute form is
documented in [`docs/cfg-attributes.md`](./cfg-attributes.md).

## Primitive Module

The `primitive` branch exposes compiler-provided builtin bindings as ordinary
source-level symbols.

[`std/src/primitive/i32.fe`](../std/src/primitive/i32.fe) currently re-exports:

- `i32`
- `i32_add`
- `i32_sub`
- `i32_mul`
- `i32_div`
- `i32_mod`

## IO Module

The `io` branch exposes the builtin `IO` effect as an ordinary public symbol.

[`std/src/io.fe`](../std/src/io.fe) currently re-exports:

- `IO`

The `hello-world` testcase shows this binding being imported as `std::io::IO`
and used in a function declaration with `#with IO`. In this context, `#with`
attaches an Algebraic Effects `Effect` to the function declaration.

## Mathematics Modules

The `math` branch currently exposes `nat` and `eq`.

### Natural Numbers

[`std/src/math/nat/nat.fe`](../std/src/math/nat/nat.fe) defines a public type
`Nat` with the public constructors:

- `zero`
- `succ`

[`std/src/math/nat/nat_add.fe`](../std/src/math/nat/nat_add.fe) defines:

- `nat_add`
- `nat_add_zero_x_eq_x`
- `nat_add_x_zero_eq_x`

These declarations show that the standard library surface is not limited to
primitive bindings. It can also expose ordinary functions and proof-oriented
artifacts as public symbols.

The declaration forms used for these exports are summarized in
[`docs/declaration-forms.md`](./declaration-forms.md).

### Equality

[`std/src/math/eq/eq.fe`](../std/src/math/eq/eq.fe) defines a public
proposition `Eq[u]` with the public constructor:

- `eq_refl`

## Package-Qualified References Inside `std`

Standard library modules may reference each other through package-qualified
paths rooted at `#package::`.

For example, [`std/src/math/nat/nat_add.fe`](../std/src/math/nat/nat_add.fe)
imports `Eq` and `Nat` through fully qualified package paths before re-exported
definitions depend on them.
