# Structs and Algebraic Data Types

## Structs

Use `#struct` to define a type with named fields.

```felis
#struct Span : Type[0] {
    start : i32,
    end : i32,
}
```

Construct a value with `TypeName { field = value }`.

```felis
#let span : Span = Span { start = 1i32, end = 2i32 };
```

Access fields with `value.field`.

```felis
#let sum : i32 = i32_add span.start span.end;
```

Felis also supports `#struct(rc)`. It is declared like an ordinary struct and can be used with field access and references.

```felis
#struct(rc) Point : Type[0] {
    x : i32,
    y : i32,
}
```

## Algebraic Data Types

Use `#type` to define a type with multiple constructors.

```felis
#type Value : Type[0] {
    single : i32 -> Value,
    pair : i32 -> i32 -> Value,
}
```

Call constructors with `TypeName::constructor`.

```felis
#let value : Value = Value::pair 20i32 22i32;
```

Use `#match` to inspect values.

```felis
#fn value_code : (value : Value) -> i32 {
    #match value {
        Value::single x => x,
        Value::pair x y => i32_add x y,
    }
}
```

Recursive types use `#type(rc)`.

```felis
#type(rc) List : Type[0] {
    nil : List,
    cons : i32 -> List -> List,
}
```

```felis
#fn list_sum : (list : List) -> i32 {
    #match list {
        List::nil => 0i32,
        List::cons x xs => {
            #let tail_sum : i32 = list_sum xs;
            i32_add x tail_sum
        }
    }
}
```

Use `_` in a pattern when the matched value is not needed.

```felis
#match value {
    Token::int x => x,
    _ => 0i32,
}
```
