# References and Mutability

Felis uses references to read and update values. Shared references are written as `& T`, and exclusive mutable references are written as `&^ T`.

## Shared References

Create a reference with `#letref` and `#borrow`.

```felis
#fn add1 : (value_ref : & i32) -> i32 #with IO {
    i32_add ((ref_get i32 value_ref)) 1i32
}

#let value : i32 = 41i32;
#letref value_ref : & i32 #borrow value;
#let r : i32 = add1 value_ref;
```

Read through a reference with `ref_get`. Reference reads and writes are IO-effectful operations, so functions that use them are annotated with `#with IO`.

## Mutable References

A mutable reference has type `&^ T` and is created with `#letref #excl`.

```felis
#let counter : i32 = 0i32;
#letref #excl counter_ref : &^ i32 #borrow counter;
```

Read with `ref_get` and write with `ref_set`.

```felis
#let next : i32 = i32_add ((ref_get i32 counter_ref)) 1i32;
ref_set i32 counter_ref next;
```

Primitive reference operations are exposed as builtin functions through
`std_core::primitive::reference`.

```felis
#use std_core::primitive::reference::ref_get;
#use std_core::primitive::reference::ref_set;

#let next : i32 = i32_add (ref_get i32 counter_ref) 1i32;
ref_set i32 counter_ref next;
```

You can also replace a whole struct value through a mutable reference.

```felis
#fn set_span_ref : (span_ref : &^ Span) -> () #with IO {
    ref_set Span span_ref Span { start = 20i32, end = 22i32 };
    ()
}
```

For containers such as `Array` and `ArrayVL`, pass the array and index to `array_get` and `array_set`. Array reads, writes, and length reads also require `#with IO`.

```felis
array_set array_ref 0i32 7i32;
#let first : i32 = array_get array_ref 0i32;
```

Use `array_len` to read the runtime length of an `ArrayVL`.

```felis
#let len : i32 = array_len arrayvl_ref;
```
