# 0.0.5

- add support for recursive types (self-referential)
- add support for mutually recursive types (defined with `and`)

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
