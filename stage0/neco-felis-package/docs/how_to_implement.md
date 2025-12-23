# How to implement

## Intent
- Represent a Felis package as a name and a module tree.
- Keep the crate small and reusable across compilation phases.

## Steps
- Define a `Package<P: Phase>` struct storing `name` and `module_tree`.
- Provide a `new` constructor for convenience.
- Re-export or extend functionality only when there is a clear consumer.
