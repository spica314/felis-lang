# Repository Guidelines

## Project Structure & Module Organization
- `neco-bootstrap/` — Rust workspace with compiler crates (e.g., `neco-felis-compile`, `neco-felis-syn`, `neco-cic`).
- `examples/` — runnable Felis programs (e.g., ray-tracing). `.fe` sources.
- `testcases/` — Felis language test inputs grouped by feature.
- `docs/` — design and PTX notes.
- `ci-scripts/` — repo checks (newlines, file sizes, Rust checks). Used in CI.
- `.github/workflows/` — GitHub Actions (nightly toolchain, runs `ci-scripts/check.sh`).

## Build, Test, and Development Commands
- Setup (local): `rustup toolchain install nightly && rustup component add rustfmt clippy`.
- Workspace build: `cd neco-bootstrap && cargo build --workspace`.
- Format check: `neco-bootstrap/ci-scripts/check.sh` or `cargo fmt --all -- --check`.
- Lint: `cargo clippy --all-targets --all-features -- -D warnings`.
- Test (CPU): `cargo test --workspace --offline`.
- Test (GPU/PTX): `cargo test --workspace --offline --features neco-felis-compile/has-ptx-device`.
- Full repo checks: `ci-scripts/check.sh [--fix]`.
- Coverage (HTML): `neco-bootstrap/ci-scripts/generate-coverage-report.sh`.
- Run compiler example: `cargo run -p neco-felis-compile -- ./examples/ray-tracing-in-one-weekend/main.fe -o a.out --ptx`.

## Coding Style & Naming Conventions
- Rustfmt enforced; 4-space indentation; max line length per default rustfmt.
- Naming: crates/modules `snake_case`; types/traits `PascalCase`; functions/vars `snake_case`.
- Keep files reasonably small; CI flags Rust files ≥1500 lines.
- Prefer explicit `use` paths; avoid unused imports (clippy enforced).

## Testing Guidelines
- Use `cargo test` for unit/integration tests. Place Rust tests in `tests/` or `src/*/mod tests`.
- Snapshot tests via `insta` are available in the workspace; update snapshots intentionally.
- Add Felis `.fe` samples under `testcases/` when covering language features.

## Commit & Pull Request Guidelines
- Commits: imperative mood, scoped and minimal (e.g., "parser: handle tuple types").
- Include rationale and references to issues (e.g., `Fixes #123`).
- Run `ci-scripts/check.sh` locally before pushing.
- PRs: clear description, test coverage notes, how to reproduce, and any perf/compat implications. Add screenshots or artifacts for example outputs when helpful (e.g., `image.png`).

## Notes
- CI uses Rust nightly; keep code compatible with the pinned nightly where possible.
- PTX/GPU paths are optional and auto-detected in scripts; prefer CPU paths for portability.
