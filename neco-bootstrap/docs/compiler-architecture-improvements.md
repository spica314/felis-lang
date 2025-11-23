# neco-bootstrap compiler architecture improvement ideas

## Current shape (context)
- The parsing crate already organizes syntax into distinct modules for positions, items, terms, procedural terms, and statements, suggesting the groundwork for a staged frontend, but downstream phases still operate directly on the parsed structures.
- The x86-64 and PTX backends are driven by a single `AssemblyCompiler` that accumulates assembly text in strings and interleaves PTX pre-scan and emission responsibilities in one struct, limiting the ability to share middle-end analyses or swap codegen strategies.

## Suggested improvements
1. **Introduce a structured mid-level IR**
   - Lower parsed items and procedural terms into a typed, control-flow aware IR (e.g., SSA-style blocks with explicit variable bindings) before emitting assembly. This enables reusable analyses (liveness, borrow checks, pattern exhaustiveness) and separates lowering from textual emission. A printer/renderer can then target x86-64 and PTX backends independently.
2. **Pass manager and pipeline boundaries**
   - Make the major phases explicit (parse → name resolution/renaming → scoping → type checking → IR lowering → optimization → backend), threading a shared `Context` object through the crates. Each phase should consume/produce well-defined data (AST, symbol tables, typed IR) to simplify incremental recompilation and testing.
3. **Dedicated diagnostic engine**
   - Centralize error/diagnostic reporting with structured spans, fix-it hints, and phase tags. Plugging the engine into parsing (`neco-felis-syn`), renaming, and codegen reduces ad-hoc error handling and allows richer tooling (language server, IDE integrations) without backend coupling.
4. **Backend abstraction with capability negotiation**
   - Factor backend-specific details into trait objects (e.g., `Backend` with hooks for function emission, memory layout, and builtins). Provide separate implementations for x86-64 and PTX, and allow future targets (SIMD CPUs, alternate GPU ISAs) to share the same IR pipeline. Capability flags can steer lowering choices (stack allocation vs. device memory) without duplicating control-flow logic.
5. **Resource modeling for PTX kernels**
   - Build an intermediate representation of kernel launch plans (grid/block dimensions, buffer bindings) and feed it to a dedicated PTX generator instead of mixing it with host-side assembly generation. This makes it easier to validate resource usage, insert safety checks, and potentially auto-tune launch parameters.
