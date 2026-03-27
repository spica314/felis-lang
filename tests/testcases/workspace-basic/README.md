# workspace-basic

This fixture captures the workspace manifest shape for `neco-package.json`.

## Layout

- The workspace root manifest lives at `./neco-package.json`.
- The workspace root is not itself a package.
- `workspace-app/` is a binary package.
- `workspace-lib/` is a library package.

## Workspace Manifest

The root manifest declares the workspace through a `workspace.members` field.

## Package Dependencies

`workspace-app` declares a dependency on `workspace-lib` through:

```json
"dependencies": {
    "workspace-lib": {
        "workspace": true
    }
}
```
