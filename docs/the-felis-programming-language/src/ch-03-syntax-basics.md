# Syntax Basics

Many declarations and control forms in Felis use `#`-prefixed keywords.

```felis
#use std_core::io::IO;
#entrypoint main;

#fn main : () #with IO {
    #let x : i32 = 42i32;
    ()
}
```

## Imports

Use `#use` to bring a name into scope.

```felis
#use std_core::primitive::i32::i32;
#use std_core::primitive::i32::i32_add;
```

Public names from packages in the same workspace, or from dependency packages, use the same form.

```felis
#use workspace-lib::choice_code;
```

## Bindings

Bind ordinary values with `#let name : Type = expression;`.

```felis
#let a : i32 = 3i32;
#let b : i32 = 7i32;
#let sum : i32 = i32_add a b;
```

Use `<-` when binding a value produced by an effect such as IO.

```felis
#let stdout : FileDescriptor <- IO::stdout;
#let _ : () <- IO::sys_write stdout message 14i32;
```

Bind to `_` when the returned value is intentionally ignored.

## Expressions and Blocks

The final expression in a block becomes the value of that block. `()` is the unit value.

```felis
#fn f : () {
    ()
}
```

Function calls use prefix notation.

```felis
i32_add 20i32 22i32
```

Parentheses can group an argument expression.

```felis
i32_add ((ref_get i32 sum_ref)) 1i32
```

## Comments

Felis supports line comments and block comments.

```felis
#use std_core::io::IO; // line comment

/* block comment */
#entrypoint main;
```
