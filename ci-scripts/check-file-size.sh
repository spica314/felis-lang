#!/bin/bash

# Find large Rust files (>= 1500 lines) and display their line counts
# Exit with error code if any files exceed the limit

echo "Checking for Rust files with >= 1500 lines..."

large_files_found=false

while IFS= read -r -d '' file; do
    lines=$(wc -l < "$file")
    if [ "$lines" -ge 1500 ]; then
        echo "$file: $lines lines"
        large_files_found=true
    fi
done < <(find . -path './stage0/target' -prune -o -name '*.rs' -print0)

if [ "$large_files_found" = true ]; then
    exit 1
else
    echo "No Rust files exceed 1500 lines."
    exit 0
fi
