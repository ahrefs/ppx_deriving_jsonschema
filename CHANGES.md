# Unreleased

- **breaking**: allow extra fields by default in generated record schemas (`"additionalProperties": true`), matching the Melange JSON deriver which ignores unknown object keys; add `[@@jsonschema.disallow_extra_fields]` (and `[@jsonschema.disallow_extra_fields]` on inline records in variants) to opt back into strict objects (`"additionalProperties": false`); `[@@jsonschema.allow_extra_fields]` is still accepted for backwards compatibility but is now a no-op, and using it together with `disallow_extra_fields` is rejected

# 0.0.8

- add runtime primitive schema modules under `Ppx_deriving_jsonschema_runtime.Primitives` for Yojson and Melange JSON backends; generated code now uses the selected primitive bindings, including backend-specific `int64` handling and Melange JSON `result` schemas
- add `Ppx_deriving_jsonschema_runtime.declassify` and simplify `[@jsonschema.default]` serialization so non-primitive and parametric default values can be converted through their generated `*_to_json` functions
- fix `type nonrec` and other non-recursive type declarations so aliases are not treated as recursive self-references
- add `[@@jsonschema.compact_variants]` support on polymorphic variant type declarations
- document primitive modules, result schemas, `compact_variants`, and the completed `[@jsonschema.attrs]` example in the README
- update GitHub Actions used by build and documentation workflows

# 0.0.7

- add `[@@jsonschema.compact_variants]` attribute on variant type declarations to render unit constructors as plain string constants (`{ "const": "Name" }`) instead of single-element tuple arrays; constructors with arguments keep the tuple array encoding
- add `[@@jsonschema.format]` attribute to add a format to a type or a field; now also supported on core types (e.g. variant payloads: `A of (string[@jsonschema.format "date-time"])`); validated to only apply to `string` and `bytes` types
- add `[@@jsonschema.description]` attribute to add a description to a type or a field; now also supported on core types (e.g. variant payloads and inline type annotations) and on polymorphic variant tags
- add `~ocaml_doc` flag on `[@@deriving jsonschema]` to use `ocaml.doc` attributes (i.e. `(** ... *)` doc comments) as a fallback for `[@@jsonschema.description]` when the explicit annotation is absent; off by default
- add `[@jsonschema.attrs]` composite attribute to bundle multiple schema annotations in a single record expression (e.g. `[@jsonschema.attrs { maximum = 100; minimum = 0; description = "Score out of 100" }]`); supported on core types, label declarations, and type declarations
- add `[@jsonschema.default <value>]` attribute on record fields to set a JSON Schema `"default"` value; fields with a default are excluded from `required`; primitive literals (`int`, `int32`, `nativeint`, `float`, `string`, `bytes`, `bool`, and their `option`, `list`, `array` variants) are serialized automatically; non-primitive types (custom variants, records, etc.) require a `<type>_to_json : <type> -> Yojson.Basic.t` function to be in scope (e.g. via `[@@deriving json]`); also accepts the `[@default]` shorthand for compatibility with melange-json
- make option types nullable in generated JSON Schema (e.g. `int option` → `{"type":["integer","null"]}`); add `[@@jsonschema.option]` attribute to opt a field out of `required` without making its type nullable
- fix schema generation for parametric recursive types (e.g. `type 'a t = ... | B of 'a t`)
- fix broken `$ref` scoping when a recursive type uses a parametric recursive type with itself as a type argument (e.g. `type t = ... | N of t wrapper` where `wrapper` is also recursive); inner `$defs` are now hoisted into the root schema's `$defs` namespace
- fix generated code to qualify all stdlib references (`List.filter`, `List.assoc_opt`, `List.mem_assoc`, `List.map`, `Array.to_list`, `Array.map`) with `Stdlib.` so it compiles in scopes that shadow the stdlib (e.g. `open Base`, `open Core`, `open Containers`)
- refactor runtime using `server-reason-react`'s `browser_only` ppx to share more code between the native and melange runtimes
- raise lower bounds to match what the code now requires: `ocaml >= 5.0.0`, `ppxlib > 0.36.0`, `melange-json >= 2.0.0`, `melange-json-native >= 2.0.0`; declare `conf-npm` and `conf-python-3` as test dependencies so the melange cram test can run in the opam sandbox

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
