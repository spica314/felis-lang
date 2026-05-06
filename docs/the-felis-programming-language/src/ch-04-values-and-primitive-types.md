# Values and Primitive Types

Felis provides primitive types such as `i32`, `i64`, `f32`, `u8`, `bool`, `()`, `Array`, and `ArrayVL`.

## Integers

Integer literals include a type suffix.

```felis
#let a : i32 = 42i32;
#let b : i64 = 42i64;
#let c : u8 = 42u8;
```

Hex literals use the same suffix style.

```felis
#let byte : u8 = 0x41u8;
#let len : i32 = 0x2i32;
```

Negative `i64` literals are supported.

```felis
#let minus_one : i64 = -1i64;
```

Arithmetic operations are standard library functions.

```felis
#use std_core::primitive::i32::i32_add;
#use std_core::primitive::i32::i32_sub;
#use std_core::primitive::i32::i32_mul;
#use std_core::primitive::i32::i32_div;
#use std_core::primitive::i32::i32_mod;

#let x1 : i32 = i32_add 3i32 7i32;
#let x2 : i32 = i32_sub x1 4i32;
#let x3 : i32 = i32_mul x2 61i32;
#let x4 : i32 = i32_div x3 3i32;
#let x5 : i32 = i32_mod x4 80i32;
```

`i64` and `u8` provide the same style of operations, such as `i64_add` and `u8_add`.

## Floating-Point Numbers

`f32` values use decimal literals with the `f32` suffix.

```felis
#let a : f32 = 3.5f32;
#let b : f32 = 7.5f32;
```

`f32` provides arithmetic and comparison functions with the same naming style as integer primitives, except that there is no `f32_mod`.

```felis
#use std_core::primitive::f32::f32_add;
#use std_core::primitive::f32::f32_div;
#use std_core::primitive::f32::f32_gt;

#let sum : f32 = f32_add 3.5f32 7.5f32;
#let half : f32 = f32_div sum 2.0f32;
#let large : bool = f32_gt half 5.0f32;
```

Primitive conversion functions convert between `f32` and integer types.

```felis
#use std_core::primitive::f32::f32_from_i32;
#use std_core::primitive::i32::i32_from_f32;

#let x : f32 = f32_from_i32 42i32;
#let y : i32 = i32_from_f32 x;
```

## bool

Import `bool`, `true`, and `false` from the standard library.

```felis
#use std_core::primitive::bool::bool;
#use std_core::primitive::bool::true;
#use std_core::primitive::bool::false;
#use std_core::primitive::i32::i32_eq;

#let literal_true : bool = true;
#let comparison_true : bool = i32_eq 7i32 7i32;
```

Comparison functions return `bool` and can be used as `#if` conditions.

## Equality

Primitive equality is provided as standard library functions.

```felis
#use std_core::primitive::i32::i32_eq;

#let same : bool = i32_eq 42i32 42i32;
```

For structs and algebraic data types, define equality by comparing the relevant fields and constructors.

```felis
#struct Code : Type[0] {
    value : i32,
}

#fn code_eq : (left : Code) -> (right : Code) -> bool {
    i32_eq left.value right.value
}
```

```felis
#type Token : Type[0] {
    eof : Token,
    int : i32 -> Token,
}

#fn token_eq : (left : Token) -> (right : Token) -> bool {
    #match left {
        Token::eof => #match right {
            Token::eof => true,
            _ => false,
        },
        Token::int x => #match right {
            Token::int y => i32_eq x y,
            _ => false,
        },
    }
}
```

## Characters and String Literals

Character literals can be used as `u8` values.

```felis
#let c : u8 = 'f';
```

String literals are used as `& ArrayVL u8`.

```felis
#let message : & ArrayVL u8 = "Hello, world!\n";
```
