#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Check if --fix option is provided
FIX_MODE=false
if [[ "$1" == "--fix" ]]; then
    FIX_MODE=true
fi

if $FIX_MODE; then
    echo "=== Starting automatic fixes and checks ==="

    # Fix newlines
    echo "Fixing newlines..."
    "$SCRIPT_DIR/check-newlines.sh" --fix

    echo ""
    echo "=== Running all checks after fixes ==="
fi

# Check newlines
echo "Checking newlines..."
"$SCRIPT_DIR/check-newlines.sh"

# Check file sizes
echo "Checking file sizes..."
"$SCRIPT_DIR/check-file-size.sh"

# Run Rust checks in stage0
echo "Running Rust checks..."
if $FIX_MODE; then
    "$PROJECT_ROOT/stage0/ci-scripts/check.sh" --fix
else
    "$PROJECT_ROOT/stage0/ci-scripts/check.sh"
fi

if $FIX_MODE; then
    echo ""
    echo "=== All fixes applied and checks passed! ==="
else
    echo "All checks passed!"
fi
