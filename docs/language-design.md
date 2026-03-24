# Language Design

This document records high-level language design decisions and their rationale.
Detailed functional requirements remain in [`frd.json`](../frd.json), while this
file explains why those requirements exist.

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
