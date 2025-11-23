# PTXコードの知見集

## CUDAコードに対応するPTXコードの確認方法

1. 以下のようにcuファイルを作成する。ここでは`a.cu`とする。
```
extern "C" {
    __global__ void f(int *xs) {
        xs[0] = 42;
    }
}
```

2. `nvcc --ptx a.cu` とするとPTXコードに変換でき、コアになる部分を抽出すると以下のようになる。
```
.version 8.8
.target sm_52
.address_size 64

.visible .entry f(
	.param .u64 f_param_0
)
{
	.reg .b32 	%r<2>;
	.reg .b64 	%rd<3>;

	ld.param.u64 	%rd1, [f_param_0];
	cvta.to.global.u64 	%rd2, %rd1;
	mov.u32 	%r1, 42;
	st.global.u32 	[%rd2], %r1;
	ret;
}
```

## PTXコードのバリデーション

```
ptxas --compile-only --gpu-name=sm_52 {PTXファイル名}
```
が正常に終わればOK

## CUDA Driver API

### `CUresult cuInit ( unsigned int  Flags )`
doc: https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__INITIALIZE.html

Flagsは0固定
1回だけ必要

アセンブリのテンプレは以下の通り

```asm
    mov     edi, 0
    call    cuInit@PLT
    test    eax, eax
    jz      cu_init_ok
    # エラー時の処理 #
cu_init_ok:
    # 正常終了時の処理 #
```

### `CUresult cuDeviceGet ( CUdevice* device, int  ordinal )`
doc: https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__DEVICE.html#group__CUDA__DEVICE_1g8bdd1cc7201304b01357b8034f6587cb

deviceが指す先のCUdeviceに初期化して値が入る

GPU0版を使うなら cuDeviceGet(&cu_device, 0)で固定でいい

#### アセンブリ
グローバルに確保
```asm
    .align 4
    .type	__cu_device, @object
    .size	__cu_device, 4
__cu_device:
    .zero	4
```

呼び出し処理
```asm
    mov     esi, 0
    lea     rdi, __cu_device[rip]
    call    cuDeviceGet@PLT
    test    eax, eax
    jz      cu_device_get_ok
    # エラー時の処理 #
cu_device_get_ok:
    # 正常終了時の処理 #
```

### `CUresult cuCtxCreate ( CUcontext* pctx, unsigned int  flags, CUdevice dev )`
doc: https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__CTX.html#group__CUDA__CTX_1g65dc0012348bc84810e2103a40d8e2cf

なおdefineされていてcuCtxCreate_v2が本体

CUcontextが初期化されて作られる

cuCtxCreate_v2(&cu_context, 0, cu_device);
の呼び出し固定でよい

#### アセンブリ
グローバルに確保
```asm
    .globl	__cu_context
    .align 8
    .type	__cu_context, @object
    .size	__cu_context, 8
__cu_context:
    .zero	8
```

呼び出し処理
```asm
    mov     edx, DWORD PTR __cu_device[rip]
    mov     esi, 0
    lea     rdi, __cu_context[rip]
    call    cuCtxCreate_v2@PLT
    test    eax, eax
    jz      cu_ctx_create_v2_ok
    # エラー時の処理 #
cu_ctx_create_v2_ok:
    # 正常終了時の処理 #
```

### `CUresult cuModuleLoadData ( CUmodule* module, const void* image )`
doc: https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__MODULE.html#group__CUDA__MODULE_1g04ce266ce03720f479eab76136b90c0b

CUmoduleが初期化されて作られる。imageはPTXコード(null終端)文字列への参照

#### アセンブリ

グローバルに定義
```asm
    .globl	__cu_module
    .align 8
    .type	__cu_module, @object
    .size	__cu_module, 8
__cu_module:
    .zero	8
```

呼び出し方法
```asm
    lea     rsi, ptx_code[rip]
    lea     rdi, __cu_module[rip]
    call    cuModuleLoadData@PLT
    test    eax, eax
    jz      cu_module_load_data_ok
    # エラー時の処理 #
cu_module_load_data_ok:
    # 正常終了時の処理 #
```


### `CUresult cuModuleGetFunction ( CUfunction* hfunc, CUmodule hmod, const char* name )`
doc: https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__MODULE.html#group__CUDA__MODULE_1ga52be009b0d4045811b30c965e1cb2cf

CUmoduleから指定した名前の関数を取り出す。CUfunctionに初期化されて入る

#### アセンブリ

```asm
    .globl	__cu_function
    .align 8
    .type	__cu_function, @object
    .size	__cu_function, 8
__cu_function:
    .zero	8
```

```asm
    lea     rdx, ptx_function_name[rip]
    lea     rsi, QWORD PTR __cu_module[rip]
    lea     rdi, __cu_function[rip]
    call    cuModuleGetFunction@PLT
    test    eax, eax
    jz      cu_module_get_function_ok
    # エラー時の処理 #
cu_module_get_function_ok:
    # 正常終了時の処理 #
```

### `CUresult cuMemAlloc ( CUdeviceptr* dptr, size_t bytesize )`
doc: https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__MEM.html#group__CUDA__MEM_1gb82d2a09844a58dd9e744dc31e8aa467

なおdefineされていてcuMemAlloc_v2が本体
CUdeviceptrが指す先に指定したサイズの領域を確保した領域へのポインタ

#### アセンブリ

グローバルに定義
```asm
    .globl	device_ptr
    .align 8
    .type	device_ptr, @object
    .size	device_ptr, 8
device_ptr:
    .zero	8
```

```asm
    mov     rsi, 65536
    lea     rdi, device_ptr[rip]
    call    cuMemAlloc_v2@PLT
    test    eax, eax
    jz      cu_mem_alloc_v2_ok
    # エラー時の処理 #
cu_mem_alloc_v2_ok:
    # 正常終了時の処理 #
```

### `CUresult cuMemcpyHtoD ( CUdeviceptr dstDevice, const void* srcHost, size_t ByteCount ) `
doc: https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__MEM.html#group__CUDA__MEM_1g4d32266788c440b0220b1a9ba5795169

なおdefineされていてcuMemcpyHtoD_v2が本体
ホスト上のメモリからdstDeviceが指す領域にコピー

```asm
    lea     rdi, device_ptr[rip]
    mov     rsi, QWORD PTR [rbp - 8 - 0]
    mov     rdx, 65536
    call    cuMemcpyHtoD_v2@PLT
    test    eax, eax
    jz      cu_memcpy_h_to_d_v2_ok:
    # エラー時の処理 #
cu_memcpy_h_to_d_v2_ok:
    # 正常終了時の処理 #
```

### `CUresult cuLaunchKernel ( CUfunction f, unsigned int  gridDimX, unsigned int  gridDimY, unsigned int  gridDimZ, unsigned int  blockDimX, unsigned int  blockDimY, unsigned int  blockDimZ, unsigned int  sharedMemBytes, CUstream hStream, void** kernelParams, void** extra )`
doc: https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__EXEC.html#group__CUDA__EXEC_1gb8f3dc3031b40da29d5f9a7139e52e15

基本的にsharedMemBytes, hStream, extraは0(NULL)にしておけばOK。
kernelParamsはCUdeviceptrの参照の配列への参照

```asm
    mov     rdi, QWORD PTR __cu_function[rip]
    mov     rsi, 256 # gridDimX
    mov     rdx, 1   # gridDimY
    mov     rcx, 1   # gridDimZ
    mov     r8,  256 # blockDimX
    mov     r9, 1    # blockDimY
    push    0        # void** extra
    lea     rax, [rbp - 8 - 0]
    push    rax      # kernelParams
    push    0        # hStream
    push    0        # sharedMemBytes
    push    1        # blockDimZ
```

### `CUresult cuMemcpyDtoH ( void* dstHost, CUdeviceptr srcDevice, size_t ByteCount )`
doc: https://docs.nvidia.com/cuda/cuda-driver-api/group__CUDA__MEM.html#group__CUDA__MEM_1g3480368ee0208a98f75019c9a8450893

なおdefineされていてcuMemcpyDtoH_v2が本体
srcDeviceからホスト上のメモリにコピー

```asm
    mov     rdi, QWORD PTR [rbp - 8 - 0]
    mov     rsi, QWORD PTR device_ptr[rip]
    mov     rdx, 65536
    call    cuMemcpyDtoH_v2@PLT
    test    eax, eax
    jz      cu_memcpy_d_to_h_v2_ok:
    # エラー時の処理 #
cu_memcpy_d_to_h_v2_ok:
    # 正常終了時の処理 #
```

### `CUresult cuMemFree ( CUdevicept dptr )`
doc: https://developer.download.nvidia.com/compute/DevZone/docs/html/C/doc/html/group__CUDA__MEM_g89b3f154e17cc89b6eea277dbdf5c93a.html

なおdefineされていてcuMemFree_v2が本体
dptrが指す先のデバイス上のメモリを解放

```asm
    mov     rdi, QWORD PTR [rbp - 8 - 0]
    mov     rsi, QWORD PTR device_ptr[rip]
    mov     rdx, 65536
    call    cuMemFree_v2@PLT
    test    eax, eax
    jz      cu_mem_free_v2_ok:
    # エラー時の処理 #
cu_mem_free_v2_ok:
    # 正常終了時の処理 #
```
