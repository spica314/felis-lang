# workspace-type-dependency

This fixture captures workspace lowering for types declared in another member
package. The app constructs and matches both a plain `#type` value and a nested
`#type(rc)` value from `workspace-lib`.
