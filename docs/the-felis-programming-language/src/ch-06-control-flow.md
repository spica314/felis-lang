# Control Flow

## if

`#if` branches on a `bool` condition.

```felis
#if i32_eq 3i32 3i32 {
    #let _ : () <- IO::exit 42i32;
};
```

`#else` and `#else #if` are also supported.

```felis
#if i32_eq 3i32 4i32 {
    #let _ : () <- IO::exit 1i32;
} #else #if i32_eq 5i32 5i32 {
    #let _ : () <- IO::exit 42i32;
} #else {
    #let _ : () <- IO::exit 2i32;
};
```

When `#if` is used as a statement, the block is followed by `;`.

## loop

`#loop` creates an infinite loop. Use `#break` to leave it.

```felis
#loop {
    #if i32_eq ((ref_get i32 counter)) 10i32 {
        #break;
    };
};
```

Use `#continue` to skip to the next iteration.

```felis
#loop {
    #let new_counter : i32 = i32_add ((ref_get i32 counter)) 1i32;
    ref_set i32 counter new_counter;
    #if i32_eq ((ref_get i32 counter)) 5i32 {
        #continue;
    };
};
```

Updating values inside a loop is usually done through mutable references.
