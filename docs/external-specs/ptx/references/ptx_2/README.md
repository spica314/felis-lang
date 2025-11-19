# PTX Compilation Example 1

This example demonstrates PTX (Parallel Thread Execution) code compilation and execution using NVIDIA CUDA.

## Overview

This project showcases the compilation pipeline from CUDA source code to PTX assembly and its integration with a C host program. It serves as a reference for understanding how PTX code generation and compilation works in the CUDA ecosystem.

## Files

- `b.cu` - CUDA kernel source code containing a simple function `f`
- `a.c` - Host C program that loads and executes the PTX module
- `build.sh` - Build script that orchestrates the compilation process

## Build Process

Run the build script to generate the compilation artifacts:

```bash
./build.sh
```

This will generate:
- `b.ptx` - PTX assembly code compiled from the CUDA kernel
- `a.s` - Assembly code generated from the C host program
- `a.out` - Final executable that can run the PTX code

## Compilation Pipeline

1. **CUDA to PTX**: `nvcc --ptx b.cu` compiles the CUDA kernel to PTX assembly
2. **C to Assembly**: `gcc -S a.c -I/opt/cuda/include -masm=intel` generates Intel assembly from the host C code
3. **Final Linking**: `gcc -o a.out a.s /opt/cuda/lib64/stubs/libcuda.so` links the assembly with CUDA runtime libraries

## Usage

After building, execute the program:

```bash
./a.out
```

The program will output the result of the GPU kernel execution.

## Purpose

This example serves as a reference implementation for:
- Understanding PTX code generation from CUDA sources
- Learning the host-side CUDA Driver API usage
- Demonstrating the compilation workflow for GPU computing applications
