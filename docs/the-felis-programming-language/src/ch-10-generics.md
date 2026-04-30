# Generics

Felis expresses generics by passing types as arguments. The type of a type is written as `Type[0]`.

## Generic Functions

An identity function can be written like this:

```felis
#fn id : (t : Type[0]) -> (value : t) -> t {
    value
}
```

Call it by passing the type first, then the value.

```felis
#let value : i32 = id i32 42i32;
```

## Generic Types

Type definitions can also take type parameters.

```felis
#type LocalResult : (t : Type[0]) -> (e : Type[0]) -> Type[0] {
    ok : (t : Type[0]) -> (e : Type[0]) -> t -> LocalResult t e,
    err : (t : Type[0]) -> (e : Type[0]) -> e -> LocalResult t e,
}
```

In a match, constructor type arguments appear in the pattern. Use `_` when they are not needed.

```felis
#fn result_code : (value : LocalResult i32 i32) -> i32 {
    #match value {
        LocalResult::ok _ _ x => x,
        LocalResult::err _ _ _ => 0i32,
    }
}
```

The standard library provides `Option` and `Result`.

```felis
#use std_core::option::Option;
#use std_core::result::Result;

#let maybe_code : Option i32 = Option::some i32 42i32;
#let error_code : Result i32 i32 = Result::err i32 i32 42i32;
```

Use `#match` to inspect `Option` and `Result`.

```felis
#match maybe_code {
    Option::some _ x => x,
    Option::none _ => 0i32,
}
```
