# Arrays, Slices, and Strings

## Array

Fixed-size arrays are written as `Array T N`. Examples that allocate arrays through IO receive a mutable reference, such as `&^ Array T N`.

```felis
#let array_ref : &^ Array i32 4i32 <- IO::array_new i32 4i32;
array_ref .> set 0i32 7i32;
array_ref .> set 1i32 14i32;
#let first : i32 = array_ref .> get 0i32;
```

Array references can be used as function parameters.

```felis
#fn sum3 : (array_ref : &^ Array i32 4i32) -> i32 {
    #let a0 : i32 = array_ref .> get 0i32;
    #let a1 : i32 = array_ref .> get 1i32;
    i32_add a0 a1
}
```

## Slice

`Slice T` represents a runtime-sized region. Allocate one with `IO::slice_new`.

```felis
#let slice_ref : &^ Slice u8 <- IO::slice_new u8 42i32;
#let len : i32 = slice_ref .> len;
```

String literals can be used as `Slice u8`.

```felis
#let path : Slice u8 = "message.txt";
```

## DynArray

`std_core::primitive::array::DynArray` is a dynamic array. Operations take the element type explicitly.

```felis
#use std_core::primitive::array::DynArray;
#use std_core::primitive::array::dyn_array_get;
#use std_core::primitive::array::dyn_array_len;
#use std_core::primitive::array::dyn_array_push;

#let bytes_slice_ref : &^ Slice u8 <- IO::slice_new u8 1i32;
#let bytes : DynArray u8 = DynArray::dyn_array u8 bytes_slice_ref 0i64;
dyn_array_push u8 bytes 1u8;
dyn_array_push u8 bytes 2u8;
#let first : u8 = dyn_array_get u8 bytes 0i32;
#let len : i32 = dyn_array_len u8 bytes;
```

Use `dyn_array_capacity` to read the current capacity.

## String

`std_core::string::String` can be built from a sequence of `u8` values.

```felis
#use std_core::string::String;
#use std_core::string::string_from_dyn_array;
#use std_core::string::string_get_u8;
#use std_core::string::string_len;

#let string : String = string_from_dyn_array bytes;
#let first : u8 = string_get_u8 string 0i32;
#let len : i32 = string_len string;
```

Use `string_from_slice_parts` to create a string from existing slice storage. Use `string_with_len` to create a length-bounded string view.

```felis
#let slice_ref : &^ Slice u8 <- IO::slice_new u8 4i32;
#let empty : String = string_from_slice_parts slice_ref 4i32 0i32;
#let name_len : i32 = 0i32;
#letref #excl name_len_ref : &^ i32 #borrow name_len;
string_push_u8_unchecked empty name_len_ref 'f';
#let name : String = string_with_len empty (name_len_ref .> get);
```
