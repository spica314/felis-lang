# Declaration Forms

This document records declaration forms that are already used by the Felis
standard library.

It is intentionally narrow: it describes the currently documented source forms
that are observable in [`std/`](../std/) rather than trying to define the whole
language grammar at once.

## Declaration Attributes

Declarations can be preceded by attribute lines such as `#[cfg(...)]`.

The currently documented conditional-compilation attribute is described in
[`docs/cfg-attributes.md`](./cfg-attributes.md).

## Public Function Declarations

`#fn` declares a function. `#pub #fn` declares a public function that becomes
part of the enclosing module surface.

Example:

```felis
#pub #fn nat_add : (x : Nat) -> (y : Nat) -> Nat {
    #match x {
        Nat::zero => y
        Nat::succ p => {
            Nat::succ (nat_add p y)
        }
    }
}
```

The documented shape is:

```felis
#fn <name> : <type> {
    <body>
}
```

or, when exported:

```felis
#pub #fn <name> : <type> {
    <body>
}
```

## Public Type Declarations

`#type(...)` declares a type. The modifier part in parentheses is optional.
The standard library currently uses `#type(rc)` for algebraic-style type
declarations with constructors listed in a declaration body.

Example:

```felis
#pub #type(rc) Nat : Type[0] {
    zero : Nat,
    succ : Nat -> Nat,
}
```

The documented shape is:

```felis
#type <name> : <type> {
    <constructors>
}
```

or, with an optional modifier:

```felis
#type(<modifier>) <name> : <type> {
    <constructors>
}
```

The documented observable properties are:

- A declaration has a name and a type-level annotation after `:`.
- The body lists public constructors.
- `#pub #type(rc)` makes the declared type part of the module's public surface.
- The `rc` modifier enables automatic reference counting for values of the
  declared type.

This means `#type(rc)` is the declaration form for types that participate in
reference-counted lifetime management.

## Public Theorem Declarations

`#theorem` declares a named theorem. `#pub #theorem` makes that theorem part of
the enclosing module's public surface.

Example:

```felis
#pub #theorem nat_add_zero_x_eq_x : #forall x : Nat, Eq[0] Nat (nat_add Nat::zero x) x {
    #fn proof : (x : Nat) -> Eq[0] Nat (nat_add Nat::zero x) x {
        Eq[0]::eq_refl Nat x
    }
}
```

The documented shape is:

```felis
#theorem <name> : <statement> {
    <proof-related declarations or expressions>
}
```

or, when exported:

```felis
#pub #theorem <name> : <statement> {
    <proof-related declarations or expressions>
}
```

The standard library currently uses `#forall` inside theorem statements to bind
universally quantified variables.

## Public Proposition Declarations

`#prop` declares a proposition-valued entity. `#pub #prop` makes that
declaration part of the enclosing module's public surface.

Example:

```felis
#pub #prop Eq[u] : (t : Type[u]) -> t -> t -> Prop[0] {
    eq_refl : (t : Type[u]) -> (x : t) -> Eq[u] t x x,
}
```

The documented observable properties are:

- A proposition declaration has a name and a type annotation.
- The body can list public constructors.
- `#pub #prop` exposes the proposition through the module surface.

## Match Expressions

`#match` performs case analysis on a value.

Example:

```felis
#match x {
    Nat::zero => y
    Nat::succ p => {
        Nat::succ (nat_add p y)
    }
}
```

The documented shape is:

```felis
#match <scrutinee> {
    <pattern> => <result>
    ...
}
```

The standard library currently uses these observable pattern forms:

- A constructor name by itself, such as `Nat::zero`.
- A constructor name followed by bound variables, such as `Nat::succ p`.
- A constructor name followed by multiple subpatterns, such as
  `Eq[0]::eq_refl _ p2`.
- The wildcard pattern `_`, which matches a value without binding a name.

This means the current documented pattern shape can be summarized as:

```felis
<constructor>
<constructor> <subpattern> ...
_
```

This document still does not attempt to define the full pattern grammar beyond
the constructor-style and wildcard patterns that appear in the current standard
library.

## Let Bindings

`#let` introduces a local binding inside a declaration body or expression block.

Example:

```felis
#let s = proof p;
```

The documented shape is:

```felis
#let <name> = <value>;
```

## Universal Quantification

`#forall` introduces universally quantified binders in theorem statements.

Example:

```felis
#forall x : Nat, Eq[0] Nat (nat_add Nat::zero x) x
```

The documented shape is:

```felis
#forall <name> : <type>, <statement>
```

## Scope

These supporting forms are documented here only to the extent needed to read
the current standard-library declarations. A fuller expression-level syntax
document can be added separately when needed.
