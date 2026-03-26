# `cfg` Attributes

This document records the currently documented conditional-compilation
attribute form used by the Felis standard library.

It is intentionally narrow: it describes the observable `cfg` forms that are
already present in [`std/`](../std/) rather than attempting to define a full
attribute system.

## Conditional Declaration Inclusion

`#[cfg(...)]` attaches a configuration predicate to the following declaration.
When the predicate evaluates to true, the declaration is included. When the
predicate evaluates to false, the declaration is omitted.

Example:

```felis
#[cfg(not(feature="bootstrap"))]
#pub #mod math;
```

The documented shape is:

```felis
#[cfg(<predicate>)]
<declaration>
```

The documented observable properties are:

- The attribute applies to the declaration that immediately follows it.
- A false predicate removes the attached declaration from the observable module
  surface.
- A true predicate leaves the attached declaration available as if it had been
  written without the attribute.

## Predicates

The standard library currently uses these observable predicate forms:

```felis
feature="<name>"
not(<predicate>)
```

The documented observable properties are:

- `feature="<name>"` tests whether a named feature is enabled.
- `not(<predicate>)` inverts the result of another predicate.

This means the currently documented standard-library usage can express
feature-based inclusion and simple negation.
