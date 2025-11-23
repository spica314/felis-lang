#!/bin/bash

set -euo pipefail

# Resolve directories
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
WORKSPACE_DIR="$SCRIPT_DIR/../../neco-bootstrap"

# Build and run for multiple images
# images=(image1 image2)
images=(image1 image2)

# Build Felis programs using the compiler from the workspace, then run and convert
pushd "$WORKSPACE_DIR" >/dev/null
for img in "${images[@]}"; do
  # Compile
  cargo run -p neco-felis-compile -- "$SCRIPT_DIR/${img}.fe" -o "$SCRIPT_DIR/${img}.out"
  # Run generated binary and convert output image
  "$SCRIPT_DIR/${img}.out" > "$SCRIPT_DIR/${img}.pnm"
  pnm2png "$SCRIPT_DIR/${img}.pnm" "$SCRIPT_DIR/${img}.png"
done
popd >/dev/null
