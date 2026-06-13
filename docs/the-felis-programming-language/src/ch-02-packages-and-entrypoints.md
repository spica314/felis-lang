# Packages and Entry Points

A Felis package is described by a `neco-package.json` file. Executable packages list their source entry files in `felis-bin-entrypoints`.

```json
{
    "name": "hello-world",
    "felis-bin-entrypoints": ["src/hello-world.fe"]
}
```

Test entrypoint files are listed in `felis-test-entrypoints`. `neco test` builds
and runs each listed entrypoint, and succeeds when all of them exit with status
code 0.

```json
{
    "name": "hello-world",
    "felis-test-entrypoints": ["src/hello-world-test.fe"]
}
```

Inside the source file, use `#entrypoint` to choose the function that should run.

```felis
#entrypoint main;

#fn main : () {
    ()
}
```

`main` is an ordinary function. If it performs IO, add `#with IO`.

```felis
#use std_core::io::IO;

#entrypoint main;

#fn main : () #with IO {
    ()
}
```

A workspace is declared in the root `neco-package.json` with `workspace.members`.

```json
{
    "workspace": {
        "members": [
            "workspace-app",
            "workspace-lib"
        ]
    }
}
```

A package can depend on another workspace package by adding it to `dependencies` with `workspace: true`.

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
