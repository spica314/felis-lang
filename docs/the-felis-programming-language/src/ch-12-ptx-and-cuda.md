# PTX and CUDA

Felis can compile selected functions to PTX and use CUDA driver operations from
an IO entry point. PTX code is written as ordinary Felis functions annotated
with `#with PTX`.

## PTX Functions

Import the PTX effect and device array type before writing a kernel.

```felis
#use std_core::ptx::PTX;
#use std_core::primitive::array::ArrayVLPTX;
#use std_core::primitive::i32::i32;
#use std_core::primitive::reference::ref_get_ptx;
#use std_core::primitive::reference::ref_set_ptx;

#fn copy_first : (array : ArrayVLPTX i32) -> () #with PTX {
    #let value : i32 = ref_get_ptx i32 array 0i32;
    ref_set_ptx i32 array 1i32 value;
    ()
}
```

`ArrayVLPTX T` represents device-side runtime-sized storage. PTX array reads and
writes use `ref_get_ptx` and `ref_set_ptx`; host-side `array_get` and
`array_set` are for `Array` and `ArrayVL` values outside PTX functions.

PTX functions can call other PTX functions.

```felis
#use std_core::primitive::i32::i32_add;

#fn add_one : (value : i32) -> i32 #with PTX {
    i32_add value 1i32
}

#fn kernel : (array : ArrayVLPTX i32) -> () #with PTX {
    #let value : i32 = ref_get_ptx i32 array 0i32;
    #let next : i32 = add_one value;
    ref_set_ptx i32 array 1i32 next;
    ()
}
```

## Compiling PTX

Use `#compile_ptx` to compile a PTX function into a value that can be loaded by
the CUDA driver. The value behaves like static PTX bytes and can be assigned to
`& ArrayVL u8`.

```felis
#use std_core::primitive::array::ArrayVL;

#compile_ptx kernel #to kernel_ptx;

#fn main : () #with IO {
    #let ptx : & ArrayVL u8 = kernel_ptx;
    ()
}
```

The current PTX lowering expects a compiled kernel target to use `#with PTX`,
take one named parameter, and return `()`. `ArrayVLPTX i32`, `ArrayVLPTX i64`,
`ArrayVLPTX f32`, and `ArrayVLPTX u8` are supported device array parameter
forms.

## Special Registers

The `std_core::ptx` module exposes CUDA special registers as PTX-effectful
values. Import the registers you use.

```felis
#use std_core::ptx::ctaid_x;
#use std_core::ptx::ntid_x;
#use std_core::ptx::tid_x;
#use std_core::primitive::i32::i32_add;
#use std_core::primitive::i32::i32_mul;

#fn indexed_kernel : (array : ArrayVLPTX i32) -> () #with PTX {
    #let block_offset : i32 = i32_mul ctaid_x ntid_x;
    #let index : i32 = i32_add block_offset tid_x;
    ref_set_ptx i32 array index index;
    ()
}
```

Available register values are `ctaid_x`, `ctaid_y`, `ctaid_z`, `ntid_x`,
`ntid_y`, `ntid_z`, `tid_x`, `tid_y`, and `tid_z`.

## Loading and Launching

Host-side CUDA calls are IO operations. A typical entry point initializes the
driver, creates a context, loads the compiled PTX, resolves the kernel function,
allocates device storage, copies host data to the device, launches the kernel,
and copies results back.

```felis
#use std_core::io::IO;
#use std_core::primitive::array::ArrayVL;
#use std_core::primitive::array::ArrayVLPTX;
#use std_core::primitive::array::array_get;
#use std_core::primitive::array::array_set;
#use std_core::primitive::i64::i64;
#use std_core::primitive::reference::ref_get;

#entrypoint main;

#fn main : () #with IO {
    #let init_result : i32 <- IO::cu_init 0i32;

    #let device : i32 = 0i32;
    #letref #excl device_ref : &^ i32 #borrow device;
    #let device_result : i32 <- IO::cu_device_get device_ref 0i32;
    #let device_value : i32 = ref_get i32 device_ref;

    #let ctx : i64 = 0i64;
    #letref #excl ctx_ref : &^ i64 #borrow ctx;
    #let ctx_result : i32 <- IO::cu_ctx_create_v2 ctx_ref 0i32 device_value;

    #let module : i64 = 0i64;
    #letref #excl module_ref : &^ i64 #borrow module;
    #let ptx : & ArrayVL u8 = kernel_ptx;
    #let module_result : i32 <- IO::cu_module_load_data module_ref ptx;
    #let module_value : i64 = ref_get i64 module_ref;

    #let function : i64 = 0i64;
    #letref #excl function_ref : &^ i64 #borrow function;
    #let function_result : i32 <-
        IO::cu_module_get_function function_ref module_value "kernel";
    #let function_value : i64 = ref_get i64 function_ref;

    #let host_array : ArrayVL i32 <- IO::arrayvl_new i32 2i32;
    array_set host_array 0i32 7i32;
    array_set host_array 1i32 0i32;

    #let device_array : ArrayVLPTX i32 <- IO::arrayvlptx_new i32 2i32;
    #let copy_to_result : i32 <-
        IO::arrayvl_to_ptx i32 host_array device_array 2i32;
    #let launch_result : i32 <-
        IO::cu_launch_kernel
            function_value
            device_array
            1i32 1i32 1i32
            1i32 1i32 1i32
            0i32 0i64;
    #let copy_from_result : i32 <-
        IO::arrayvl_from_ptx i32 device_array host_array 2i32;
    #let result : i32 = array_get host_array 1i32;
    ()
}
```

`IO::cu_launch_kernel` takes the function handle, one kernel argument, grid
dimensions, block dimensions, shared-memory byte count, and stream handle. The
kernel argument can be an `ArrayVLPTX` value or a supported primitive reference.
