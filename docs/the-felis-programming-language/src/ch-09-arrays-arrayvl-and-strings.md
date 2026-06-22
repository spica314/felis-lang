# Arrays, ArrayVL, and Strings

## Array

Fixed-size arrays are written as `Array T N`. Examples that allocate arrays through IO now return the array value, which can then be used as an `&^ Array T N` by borrowing.

```felis
#let array_ref : Array i32 4i32 <- IO::array_new i32 4i32;
array_set array_ref 0i32 7i32;
array_set array_ref 1i32 14i32;
#let first : i32 = array_get array_ref 0i32;
```

Array references can be used as function parameters.

```felis
#fn sum3 : (array_ref : &^ Array i32 4i32) -> i32 #with IO {
    #let a0 : i32 = array_get array_ref 0i32;
    #let a1 : i32 = array_get array_ref 1i32;
    i32_add a0 a1
}
```

## ArrayVL

`ArrayVL T` represents a runtime-sized region. Allocate one with `IO::arrayvl_new`.

```felis
#let arrayvl : ArrayVL u8 <- IO::arrayvl_new u8 42i32;
#letref #excl arrayvl_ref : &^ ArrayVL u8 #borrow arrayvl;
#let len : i32 = array_len arrayvl_ref;
```

String literals can be used as `& ArrayVL u8`.

```felis
#let bytes : & ArrayVL u8 = "message.txt";
```

## Vec

`std_core::primitive::vec::Vec` is a dynamic array. Operations take the element type explicitly.

```felis
#use std_core::primitive::vec::Vec;
#use std_core::primitive::vec::vec_get;
#use std_core::primitive::vec::vec_len;
#use std_core::primitive::vec::vec_push;

#let bytes_arrayvl : ArrayVL u8 <- IO::arrayvl_new u8 1i32;
#letref #excl bytes_arrayvl_ref : &^ ArrayVL u8 #borrow bytes_arrayvl;
#let bytes : Vec u8 = Vec::vec u8 bytes_arrayvl 0i64;
#letref #excl bytes_ref : &^ Vec u8 #borrow bytes;
vec_push u8 bytes_ref 1u8;
vec_push u8 bytes_ref 2u8;
#let current : Vec u8 <- ref_get (Vec u8) bytes_ref;
#let first : u8 = vec_get u8 current 0i32;
#let len : i32 = vec_len u8 current;
```

Use `vec_capacity` to read the current capacity.

## String

`std_core::string::String` can be built from a sequence of `u8` values.

```felis
#use std_core::string::String;
#use std_core::string::string_from_vec;
#use std_core::string::string_get_u8;
#use std_core::string::string_len;

#let string : String = string_from_vec bytes;
#let first : u8 = string_get_u8 string 0i32;
#let len : i32 = string_len string;
```

Use `string_from_arrayvl_parts` to create a string from existing `ArrayVL` storage. Use `string_with_len` to create a length-bounded string view.

```felis
#let arrayvl : ArrayVL u8 <- IO::arrayvl_new u8 4i32;
#letref #excl arrayvl_ref : &^ ArrayVL u8 #borrow arrayvl;
#let empty : String = string_from_arrayvl_parts arrayvl 4i32 0i32;
#let name_len : i32 = 0i32;
#letref #excl name_len_ref : &^ i32 #borrow name_len;
string_push_u8_unchecked empty name_len_ref 'f';
#let name : String = string_with_len empty ((ref_get i32 name_len_ref));
```
