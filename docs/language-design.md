# Language Design

This document records high-level language design decisions and their rationale.

## Keywords And Identifiers

Felis uses `#`-prefixed keywords. For example, if `#if` is a keyword, `if`
without the prefix remains available as an ordinary identifier.

### Rationale

The main goal is to keep the namespace for user-defined names separate from the
namespace for syntax.

- Compiler implementation becomes simpler because parser-reserved words do not
  need to permanently occupy common identifier spellings.
- Users can reuse familiar short names without being blocked by the language's
  control-flow or declaration keywords.
- Future syntax additions are less likely to break existing programs because
  adding a new keyword does not automatically reserve the plain word form.

### Design Direction

This choice reflects a broader design preference in Felis:

- Preserve a wide identifier space for users.
- Make syntactic forms visually explicit.
- Reduce accidental coupling between language evolution and existing code.

As the language grows, new syntax should prefer this model unless there is a
strong reason to make a construct part of the ordinary identifier namespace.

## Builtin Symbol Exposure

Felis extends the same namespace-separation idea to compiler-provided builtin
types and functions. Even when a symbol is implemented by the compiler, it
should not automatically occupy a common identifier such as `i32`.

Instead, builtin symbols are exposed through an explicit `#`-prefixed binding
form in source code or in the standard library. A representative example is:

```felis
#bind_builtin "i32" #as i32;
```

This makes the builtin symbol named `"i32"` available under the ordinary
identifier `i32`. If users want to keep that spelling for another purpose, the
language can resolve the conflict through library design and future renaming
support at import boundaries rather than by forcing compiler-reserved names into
every scope.

### Rationale

- Compiler-provided names should follow the same explicitness rules as syntax.
- Users keep control over common short identifiers instead of losing them to
  implicit builtin definitions.
- The standard library can decide which builtin capabilities become part of the
  ordinary programming model.

### Design Direction

Builtin functions related to builtin types should be exposed in the same way.
For example, addition for `i32` can be modeled by exposing a builtin function
such as `i32_add`.

Felis intentionally does not require these operations to be represented as
methods on builtin types. Keeping builtin capabilities as explicit symbols
reduces dependence on more advanced language features and keeps compiler and
specification evolution simpler.
