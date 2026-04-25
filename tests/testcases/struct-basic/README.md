# struct-basic

Source fixture package for the proposed `#struct` product type feature.

The files are intentionally split by implementation stage so compiler support
can land incrementally:

1. `struct-declaration.fe` checks parsing and declaration collection for plain
   and `rc` structs without requiring value construction or field access.
2. `struct-field-access.fe` checks named struct construction and `x.field`
   access for a plain product type.
3. `struct-rc-field-access.fe` checks that `#struct(rc)` follows the same
   allocation and payload-access direction as `#type(rc)`.

Once a stage is implemented, register the corresponding source file in the
parser and runtime fixture tests.
