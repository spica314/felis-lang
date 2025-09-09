#!/bin/bash

set -euo pipefail

# Resolve directories
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
WORKSPACE_DIR="$SCRIPT_DIR/../../neco-bootstrap"

# Build Felis program using the compiler from the workspace
pushd "$WORKSPACE_DIR" >/dev/null
cargo run -p neco-felis-compile -- "$SCRIPT_DIR/main.fe" -o "$SCRIPT_DIR/a.out" --ptx
popd >/dev/null

# Run generated binary and convert output image
"$SCRIPT_DIR/a.out" > "$SCRIPT_DIR/image.pnm"
pnm2png "$SCRIPT_DIR/image.pnm" "$SCRIPT_DIR/image.png"
