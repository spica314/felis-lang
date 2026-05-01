# neco-sat

`neco-sat` is a small Felis workspace for experimenting with SAT solving while
recording language gaps found during the implementation.

## Workspace

- `neco-package.json` defines a workspace.
- `neco-sat-bin` is the executable package.
- `neco-sat-bin/src/main.fe` contains a DIMACS CNF reader and SAT solver written
  in Felis.

## Algorithm

The current solver reads a DIMACS CNF problem from standard input and uses
exhaustive search:

1. Read up to 10000 bytes from `stdin`.
2. Skip DIMACS comment lines and the `p cnf ...` problem line.
3. Parse signed integer literals until each clause terminator `0`.
4. Encode each literal as `(variable_index, negated_flag)` in parallel arrays.
5. Store each clause as a start/end range into the literal arrays.
6. Enumerate assignment masks from `0` to `2^n - 1`.
7. Decode each mask into the assignment array.
8. A clause is satisfied when any literal evaluates to true.
9. The formula is satisfied when every clause is satisfied.

This is intentionally simpler than DPLL. It gives a concrete baseline for the
core SAT loop before adding unit propagation, pure literal elimination, and
recursive branching.

The implementation currently supports variables that appear in clauses up to
index `12`, up to `256` parsed clauses, and up to `2048` parsed literals. These
limits keep exhaustive search practical and fit the current fixed-array style.

## Expected behavior

Building and running `neco-sat-bin` with DIMACS input prints `SAT` and exits
with status `10`, prints `UNSAT` and exits with status `20`, or prints `ERROR`
and exits with status `2` when the input exceeds the supported solver limits.

Example:

```text
p cnf 1 2
1 0
-1 0
```

This input prints `UNSAT`.
