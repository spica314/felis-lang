# Standard Library Surface

This document records the currently documented public surface of the Felis
standard library.

Detailed functional requirements remain in [`frd.json`](../frd.json). This file
summarizes the observable API shape exposed from [`std/`](../std/).

## Root Modules

The standard library root currently exposes two public child modules:

- `primitive`
- `math`

The root module surface is declared in [`std/src/lib.fe`](../std/src/lib.fe).

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
