# workspace-runtime-dependency

This fixture captures the intended runtime behavior for a workspace package
that imports a value from another workspace member package and uses it from
the binary entrypoint.

It is currently expected to fail during lowering with:

`unknown entrypoint local 'f'`

The runtime test that covers this fixture is intentionally ignored until
workspace-import lowering supports this case.
