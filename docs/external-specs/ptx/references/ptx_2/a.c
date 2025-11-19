#include <stdio.h>
#include <cuda.h>
#include <assert.h>

CUdevice cu_device;
CUcontext cu_context;
CUmodule cu_module;
CUfunction cu_function;
CUdeviceptr cu_device_ptr;

char* read_entire_file(const char* filename) {
  FILE *file = fopen(filename, "r");
  if (file == NULL) {
      return NULL;
  }

  fseek(file, 0, SEEK_END);
  long file_size = ftell(file);
  fseek(file, 0, SEEK_SET);

  char *buffer = (char*)malloc(file_size + 1);
  if (buffer == NULL) {
      fclose(file);
      return NULL;
  }

  size_t bytes_read = fread(buffer, 1, file_size, file);
  buffer[bytes_read] = '\0';

  fclose(file);
  return buffer;
}

// char *buf;
char *buf = ".version 8.8\n.target sm_52\n.address_size 64\n\n           // .globl       f\n.visible .entry f(\n    .param .u64 ps_r,\n    .param .u64 ps_g,\n    .param .u64 ps_b\n)\n{\n    .reg .b64 %rd<100>;\n    .reg .b32 %r<100>;\n    .reg .b32 %f<100>;\n\n    ld.param.u64 %rd1, [ps_r];\n    ld.param.u64 %rd2, [ps_g];\n    ld.param.u64 %rd3, [ps_b];\n    cvta.to.global.u64 %rd1, %rd1;\n    cvta.to.global.u64 %rd2, %rd2;\n    cvta.to.global.u64 %rd3, %rd3;\n\n    ret;\n}\n\n";

int main() {
  // buf = read_entire_file("./b.ptx");

  CUresult result = CUDA_SUCCESS;

  result = cuInit(0);
  assert(result == CUDA_SUCCESS);

  //CUdevice device;
  result = cuDeviceGet(&cu_device, 0);
  assert(result == CUDA_SUCCESS);

  //CUcontext context;
  result = cuCtxCreate_v2(&cu_context, 0, cu_device);
  assert(result == CUDA_SUCCESS);

  // result = cuModuleLoad(&cu_module, "b.ptx");
  result = cuModuleLoadData(&cu_module, buf);
  assert(result == CUDA_SUCCESS);

  result = cuModuleGetFunction(&cu_function, cu_module, "f");
  assert(result == CUDA_SUCCESS);


  int len = 256 * 256;
  long *rs = malloc(sizeof(long) * len);
  long *gs = malloc(sizeof(long) * len);
  long *bs = malloc(sizeof(long) * len);

  CUdeviceptr d_rs;
  result = cuMemAlloc_v2(&d_rs, sizeof(long) * len);
  assert(result == CUDA_SUCCESS);

  CUdeviceptr d_gs;
  result = cuMemAlloc_v2(&d_gs, sizeof(long) * len);
  assert(result == CUDA_SUCCESS);

  CUdeviceptr d_bs;
  result = cuMemAlloc_v2(&d_bs, sizeof(long) * len);
  assert(result == CUDA_SUCCESS);

  result = cuMemcpyHtoD_v2(d_rs, rs, sizeof(long) * len);
  assert(result == CUDA_SUCCESS);

  result = cuMemcpyHtoD_v2(d_gs, gs, sizeof(long) * len);
  assert(result == CUDA_SUCCESS);

  result = cuMemcpyHtoD_v2(d_bs, bs, sizeof(long) * len);
  assert(result == CUDA_SUCCESS);

  void *args[] = {&d_rs, &d_gs, &d_bs};
  result = cuLaunchKernel(cu_function, 256, 1, 1, 256, 1, 1, 0, 0, args, 0);
  assert(result == CUDA_SUCCESS);

  result = cuMemcpyDtoH_v2(rs, d_rs, sizeof(long) * len);
  assert(result == CUDA_SUCCESS);

  result = cuMemcpyDtoH_v2(gs, d_gs, sizeof(long) * len);
  assert(result == CUDA_SUCCESS);

  result = cuMemcpyDtoH_v2(bs, d_bs, sizeof(long) * len);
  assert(result == CUDA_SUCCESS);

  for(int y = 0; y < 1; ++y) {
    for(int x = 0; x < 16; ++x) {
      int i = y * 256 + x;
      printf("%ld %ld %ld\n", rs[i], gs[i], bs[i]);
    }
  }
}
