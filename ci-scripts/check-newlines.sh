#!/bin/bash

set -e

# Check if --fix option is provided
FIX_MODE=false
if [[ "$1" == "--fix" ]]; then
    FIX_MODE=true
fi

# Determine action based on mode
if $FIX_MODE; then
    echo "Fixing trailing newlines..."
else
    echo "Checking for trailing newlines..."
fi

ERRORS_FOUND=false

# Find all text files (excluding binary files and build artifacts)
while IFS= read -r -d '' file; do

    # Check if file ends with newline
    if [[ -s "$file" ]] && [[ $(tail -c1 "$file" | wc -l) -eq 0 ]]; then
        if $FIX_MODE; then
            echo >> "$file"
            echo "Fixed: $file"
        else
            echo "ERROR: File '$file' does not end with a newline"
            ERRORS_FOUND=true
        fi
    fi
done < <(find . -type f \
  -not -path './target/*' \
  -not -path './.git/*' \
  -not -path './node_modules/*' \
  -not -path './neco-bootstrap/target/*' \
  \( -name '*.rs' -o -name '*.toml' -o -name '*.md' -o -name '*.sh' -o -name '*.fe' -o -name '*.yml' -o -name '*.yaml' \) \
  -print0)

# Exit with appropriate status
if $FIX_MODE; then
    echo "Newline fixes completed!"
elif $ERRORS_FOUND; then
    exit 1
else
    echo "All files have proper trailing newlines!"
fi
