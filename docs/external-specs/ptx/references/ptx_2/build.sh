#!/bin/bash

nvcc --ptx b.cu
gcc -S a.c -I/opt/cuda/include -masm=intel
gcc -o a.out a.s /opt/cuda/lib64/stubs/libcuda.so
