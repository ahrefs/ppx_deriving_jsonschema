# Next release
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
