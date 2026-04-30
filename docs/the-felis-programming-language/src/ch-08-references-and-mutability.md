# References and Mutability

Felis uses references to read and update values. Shared references are written as `& T`, and exclusive mutable references are written as `&^ T`.

## Shared References

Create a reference with `#letref` and `#borrow`.

```felis
#fn add1 : (value_ref : & i32) -> i32 {
    i32_add (value_ref .> get) 1i32
}

#let value : i32 = 41i32;
#letref value_ref : & i32 #borrow value;
#let r : i32 = add1 value_ref;
```

Read through a reference with `.> get`.

## Mutable References

A mutable reference has type `&^ T` and is created with `#letref #excl`.

```felis
#let counter : i32 = 0i32;
#letref #excl counter_ref : &^ i32 #borrow counter;
```

Read with `.> get` and write with `.> set`.

```felis
#let next : i32 = i32_add (counter_ref .> get) 1i32;
counter_ref .> set next;
```

You can also replace a whole struct value through a mutable reference.

```felis
#proc set_span_ref : (span_ref : &^ Span) -> () {
    span_ref .> set Span { start = 20i32, end = 22i32 };
    ()
}
```

For containers such as arrays and slices, pass an index to `.> get` and `.> set`.

```felis
array_ref .> set 0i32 7i32;
#let first : i32 = array_ref .> get 0i32;
```

Slices also provide `.> len`.

```felis
#let len : i32 = slice_ref .> len;
```
