# Run tests
echo "Running tests..."
cargo llvm-cov nextest --html --open --branch
