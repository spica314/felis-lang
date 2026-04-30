# Modules and Workspaces

## Public API

Add `#pub` to functions and types that should be usable from another package.

```felis
#pub #fn f : () {
    ()
}
```

Types and functions can both be public.

```felis
#pub #type Choice : Type[0] {
    small : i32 -> Choice,
    pair : i32 -> i32 -> Choice,
}

#pub #fn choice_code : (choice : Choice) -> i32 {
    #match choice {
        Choice::small value => value,
        Choice::pair left right => i32_add left right,
    }
}
```

## Importing from Dependency Packages

Import public names from a dependency package by using the package name as the first path segment.

```felis
#use workspace-lib::Choice;
#use workspace-lib::choice_code;
```

The imported names are used like ordinary local types and functions.

```felis
#let explicit_choice : Choice = Choice::pair 20i32 22i32;
#let explicit_code : i32 = choice_code explicit_choice;
```

## Runtime and Type Dependencies

Workspace dependencies can provide runtime values, types, constructors, and functions. Add the workspace dependency in `neco-package.json`, then import the names needed by the source file.

```json
{
    "name": "workspace-app",
    "dependencies": {
        "workspace-lib": {
            "workspace": true
        }
    },
    "felis-bin-entrypoints": ["src/main.fe"]
}
```

```felis
#use workspace-lib::default_choice;
#use workspace-lib::default_list;
#use workspace-lib::list_sum;
```
