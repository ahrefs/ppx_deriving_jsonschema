# Next release
- add `[@@jsonschema.annotation "key" "value"]` attribute to inject arbitrary key-value pairs into the generated schema (supported on type declarations and record fields)

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
