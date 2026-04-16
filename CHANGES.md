# Next release
- add `[@@jsonschema.format]` attribute to add a format to a type or a field; now also supported on core types (e.g. variant payloads: `A of (string[@jsonschema.format "date-time"])`); validated to only apply to `string` and `bytes` types
- add `[@@jsonschema.description]` attribute to add a description to a type or a field; now also supported on core types (e.g. variant payloads and inline type annotations)
- add `[@jsonschema.attrs]` composite attribute to bundle multiple schema annotations in a single record expression (e.g. `[@jsonschema.attrs { maximum = 100; minimum = 0; description = "Score out of 100" }]`); supported on core types, label declarations, and type declarations
- make option types nullable in generated JSON Schema (e.g. `int option` → `{"type":["integer","null"]}`); add `[@@jsonschema.option]` attribute to opt a field out of `required` without making its type nullable
- fix schema generation for parametric recursive types (e.g. `type 'a t = ... | B of 'a t`)
- fix broken `$ref` scoping when a recursive type uses a parametric recursive type with itself as a type argument (e.g. `type t = ... | N of t wrapper` where `wrapper` is also recursive); inner `$defs` are now hoisted into the root schema's `$defs` namespace

# 0.0.6
- update jsonschema derivation to use t instead of Yojson.Basic for type signatures

# 0.0.5

- add support for recursive types (self-referential)
- add support for mutually recursive types (defined with `and`)
- add support to polymorphic types

# 0.0.4

- add [@@json.allow\_extra\_fields] attribute

# 0.0.3

- make the default format compatible with the deriving ppxes

# 0.0.2

- add support for nativeint, bytes, ref, unit
- add ~variant_as_array for compatibility with ppx_deriving_yojson
- support variant payloads
- support polymorphic variants inheritance
- fix encoding of tuples
- change encoding of variants from enum to anyOf

# 0.0.1

- Initial alpha with basic features + @name @key @ref
