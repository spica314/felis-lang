#include <stdio.h>
#include <cuda.h>
#include <assert.h>

CUdevice cu_device;
CUcontext cu_context;
CUmodule cu_module;
CUfunction cu_function;
CUdeviceptr cu_device_ptr;

int main() {
  CUresult result = CUDA_SUCCESS;

  result = cuInit(0);
  assert(result == CUDA_SUCCESS);

  //CUdevice device;
  result = cuDeviceGet(&cu_device, 0);
  assert(result == CUDA_SUCCESS);

  //CUcontext context;
  result = cuCtxCreate_v2(&cu_context, 0, cu_device);
  assert(result == CUDA_SUCCESS);

  int xs[32] = {};
  xs[0] = 42;

  result = cuModuleLoad(&cu_module, "b.ptx");
  assert(result == CUDA_SUCCESS);

  result = cuModuleGetFunction(&cu_function, cu_module, "f");
  assert(result == CUDA_SUCCESS);

  CUdeviceptr d_x;
  result = cuMemAlloc_v2(&d_x, sizeof(xs));
  assert(result == CUDA_SUCCESS);

  result = cuMemcpyHtoD_v2(d_x, &xs, sizeof(xs));
  assert(result == CUDA_SUCCESS);

  void *args[] = {&d_x};
  result = cuLaunchKernel(cu_function, 1, 1, 1, 32, 1, 1, 0, 0, args, 0);
  assert(result == CUDA_SUCCESS);

  result = cuMemcpyDtoH_v2(&xs, d_x, sizeof(xs));
  assert(result == CUDA_SUCCESS);

  printf("%d\n", xs[0]);
}
