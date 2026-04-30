# Introduction

Felis is a programming language with prefix-style function calls, reference-based mutation, and typed effects such as IO.

This book explains the basic language elements from a user's point of view. It focuses on the constructs needed to read and write small Felis programs: packages, entry points, values, functions, control flow, data types, references, collections, generics, and modules.

A minimal program looks like this:

```felis
#entrypoint main;

#fn main : () {
    ()
}
```

`#entrypoint main;` selects the function that should run. `#fn main : ()` defines a function named `main` with no arguments and the unit return type `()`.

To write a string to standard output, import and use IO from the standard library:

```felis
#use std_core::io::IO;
#use std_core::io::FileDescriptor;

#entrypoint main;

#fn main : () #with IO {
    #let stdout : FileDescriptor <- IO::stdout;
    #let message : Slice u8 = "Hello, world!\n";
    #let _ : () <- IO::write stdout message 14i32;
    ()
}
```

Functions that use IO are annotated with `#with IO`. Bindings that receive values from IO use `<-`.
