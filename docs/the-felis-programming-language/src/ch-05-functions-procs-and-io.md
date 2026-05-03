# Functions, Procedures, and IO

Felis provides both `#fn` and `#proc`. Both forms write parameter and return types explicitly.

## Functions

Define a function with `#fn`.

```felis
#fn f : (a : i32) -> i32 {
    i32_add a 1i32
}
```

Parameters are written as `(name : Type)`. Multiple parameters are chained with `->`.

```felis
#fn add : (left : i32) -> (right : i32) -> i32 {
    i32_add left right
}
```

Call a function by writing the function name followed by its arguments.

```felis
#let x : i32 = add 20i32 22i32;
```

## Procedures

`#proc` can also return a value. It is useful for code that performs reference updates.

```felis
#proc add_into_ref : (value_ref : &^ i32) -> (delta : i32) -> i32 {
    value_ref .> set (i32_add (value_ref .> get) delta);
    i32_add (value_ref .> get) delta
}
```

## IO Functions

Functions that use IO are annotated with `#with IO`.

```felis
#use std_core::io::IO;
#use std_core::io::FileDescriptor;

#fn main : () #with IO {
    #let stdout : FileDescriptor <- IO::stdout;
    #let message : & ArrayVL u8 = "Hello\n";
    #let _ : () <- IO::write stdout message 6i32;
    ()
}
```

`IO::exit` sets the process exit code.

```felis
#let _ : () <- IO::exit 42i32;
```

File descriptors are obtained from `IO::stdin`, `IO::stdout`, and `IO::open`.

```felis
#let stdin : FileDescriptor <- IO::stdin;
#let stdout : FileDescriptor <- IO::stdout;
#let fd : FileDescriptor <- IO::open "message.txt" 0i32 0i32;
```

To read from a file or standard input, allocate an `Array u8` or `ArrayVL u8` buffer and pass it to `IO::read`. The returned value is the number of bytes read.

```felis
#let bytes_ref : Array u8 1000i32 <- IO::array_new u8 1000i32;
#let len : i32 <- IO::read stdin bytes_ref 1000i32;
#let _ : () <- IO::write stdout bytes_ref len;
```

Close files with `IO::close`.

```felis
#let path : & ArrayVL u8 = "message.txt";
#let fd : FileDescriptor <- IO::open path 0i32 0i32;
#let bytes_arrayvl : ArrayVL u8 <- IO::arrayvl_new u8 128i32;
#letref #excl bytes_arrayvl_ref : &^ ArrayVL u8 #borrow bytes_arrayvl;
#let len : i32 <- IO::read fd bytes_arrayvl_ref 128i32;
#let _ : () <- IO::close fd;
```

File-writing code can pass open flags and a mode to `IO::open`. For example, `577i32` and `420i32` are passed as the flags and mode below.

```felis
#let path : & ArrayVL u8 = "created.txt";
#let fd : FileDescriptor <- IO::open path 577i32 420i32;
#let message : & ArrayVL u8 = "open/write/close fixture\n";
#let _ : () <- IO::write fd message 25i32;
#let _ : () <- IO::close fd;
```

Command-line arguments are read with `IO::arg`. The index is passed as an `i32`.

```felis
#let path_ref : & Array u8 <- IO::arg 1i32;
#let digits_ref : & Array u8 <- IO::arg 2i32;
```
