# ppx_deriving_jsonschema

`ppx_deriving_jsonschema` is a PPX syntax extension that generates JSON schema from OCaml types.

The conversion aims to be compatible with the existing json derivers:
- https://github.com/melange-community/melange-json
- https://github.com/ocaml-ppx/ppx_deriving_yojson
- https://github.com/janestreet/ppx_yojson_conv

## Installation

```sh
opam install ppx_deriving_jsonschema
```

## `[@@deriving jsonschema]`

```ocaml
type address = {
  street: string;
  city: string;
  zip: string;
} [@@deriving jsonschema]

type t = {
  name: string;
  age: int;
  email: string option;
  address: address;
} [@@deriving jsonschema]

let schema = Ppx_deriving_jsonschema_runtime.json_schema t_jsonschema
```

Such a type will be turned into a JSON schema like this:
```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "type": "object",
  "properties": {
    "address": {
      "type": "object",
      "properties": {
        "zip": { "type": "string" },
        "city": { "type": "string" },
        "street": { "type": "string" }
      },
      "required": [ "zip", "city", "street" ]
    },
    "email": { "type": "string" },
    "age": { "type": "integer" },
    "name": { "type": "string" }
  },
  "required": [ "address", "age", "name" ]
}
```

### Conversion rules

#### Basic types

Types `int`, `int32`, `int64`, `nativeint`, `string`, `bytes`, `float`, `bool` are converted to their JSON equivalents.

Type `char` is converted to `{ "type": "string",  "minLength": 1,  "maxLength": 1}`.

Type `'a ref` is treated as `'a`.

Type `unit` is converted to `{ "type": "null" }`.

#### List and arrays

OCaml lists and arrays are converted to `{ "type": "array", "items": { "type": "..." } }`.

#### Tuples

Tuples are converted to `{ "type": "array", "prefixItems": [...] }`.

```ocaml
type t = int * string [@@deriving jsonschema]
```

```json
{
  "type": "array",
  "prefixItems": [ { "type": "integer" }, { "type": "string" } ],
  "unevaluatedItems": false,
  "minItems": 2,
  "maxItems": 2
}
```

#### Variants and polymorphic variants

By default, constructors in variants are represented as a list with one string, which is the name of the contructor. Constructors with arguments are represented as lists, the first element being the constructor name, the rest being its arguments. It reproduces the representation of `ppx_deriving_yojson` and `ppx_yojson_conv`. For example:

```ocaml
type t =
| Typ
| Class of string
[@@deriving jsonschema]
```

```json
{
  "anyOf": [
    {
      "type": "array",
      "prefixItems": [ { "const": "Typ" } ],
      "unevaluatedItems": false,
      "minItems": 1,
      "maxItems": 1
    },
    {
      "type": "array",
      "prefixItems": [ { "const": "Class" }, { "type": "string" } ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    }
  ]
}
```

Note that the implicit tuple in a polymorphic variant is flattened. This can be disabled using the `~polymorphic_variant_tuple` flag.

```ocaml
type a = [ `A of int * string * bool ] [@@deriving jsonschema]
```

```json
{
  "anyOf": [
    {
      "type": "array",
      "prefixItems": [
        { "const": "A" },
        { "type": "integer" },
        { "type": "string" },
        { "type": "boolean" }
      ],
      "unevaluatedItems": false,
      "minItems": 4,
      "maxItems": 4
    }
  ]
}
```

```ocaml
type b = [ `B of int * string * bool ] [@@deriving jsonschema ~polymorphic_variant_tuple]
```

```json
{
  "anyOf": [
    {
      "type": "array",
      "prefixItems": [
        { "const": "B" },
        {
          "type": "array",
          "prefixItems": [
            { "type": "integer" },
            { "type": "string" },
            { "type": "boolean" }
          ],
          "unevaluatedItems": false,
          "minItems": 3,
          "maxItems": 3
        }
      ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    }
  ]
}
```

A `~variant_as_string` flag is exposed to obtain a more natural representation `"anyOf": [{ "const": "..." }, ...]`. This representation does _not_ support payloads. For example:

```ocaml
type t =
| Typ
| Class of string
[@@deriving jsonschema ~variant_as_string]
```

```json
{ "anyOf": [ { "const": "Typ" }, { "const": "Class" } ] }
```

If the JSON variant names differ from OCaml conventions, it is possible to specify the corresponding JSON string explicitly using `[@name "constr"]`, for example:

```ocaml
type t =
| Typ   [@name "type"]
| Class of string [@name "class"]
[@@deriving jsonschema ~variant_as_string]
```

```json
{ "anyOf": [ { "const": "type" }, { "const": "class" } ] }
```

#### Records

Records are converted to `{ "type": "object", "properties": {...}, "required": [...], "additionalProperties": false }`.

The fields of type `option` are not included in the `required` list.

By default, additional properties are not allowed in objects. To allow additional properties, you can use either of these attribute annotations:

```ocaml
type person = {
  name : string;
  age : int;
}
[@@deriving jsonschema]
[@@allow_additional_fields]
```

```ocaml
type company = {
  name : string;
  employees : int;
}
[@@deriving jsonschema]
[@@jsonschema.allow_additional_fields]
```

Both annotations will generate a schema with `"additionalProperties": true`, allowing for additional fields not defined in the record:

```json
{
  "type": "object",
  "properties": {
    "name": { "type": "string" },
    "age": { "type": "integer" }
  },
  "required": [ "name", "age" ],
  "additionalProperties": true
}
```

When the JSON object keys differ from the ocaml field names, users can specify the corresponding JSON key implicitly using `[@key "field"]`, for example:

```ocaml
type t = {
  typ    : float [@key "type"];
  class_ : float [@key "CLASS"];
}
[@@deriving jsonschema]
```

### Inline Records in Variants

You can use the `[@jsonschema.allow_extra_fields]` attribute on a constructor with an inline record to allow additional fields in that record:

```ocaml
type inline_record_with_extra_fields =
  | User of { name : string; email : string } [@jsonschema.allow_extra_fields]
  | Guest of { ip : string }
[@@deriving jsonschema]
```

This will generate a schema that allows additional fields for the `User` variant's record but not for the `Guest` variant:

```json
{
  "anyOf": [
    {
      "type": "array",
      "prefixItems": [
        { "const": "User" },
        {
          "type": "object",
          "properties": {
            "email": { "type": "string" },
            "name": { "type": "string" }
          },
          "required": [ "email", "name" ],
          "additionalProperties": true
        }
      ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    },
    {
      "type": "array",
      "prefixItems": [
        { "const": "Guest" },
        {
          "type": "object",
          "properties": { "ip": { "type": "string" } },
          "required": [ "ip" ],
          "additionalProperties": false
        }
      ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    }
  ]
}
```

### References

Rather than inlining the definition of a type it is possible to use a [json schema `$ref`](https://json-schema.org/understanding-json-schema/structuring#dollarref) using the `[@ref "name"]` attribute. In such a case, the type definition must be passed to `Ppx_deriving_jsonschema_runtime.json_schema` as a parameter.

```ocaml
type address = {
  street : string;
  city : string;
  zip : string;
}
[@@deriving jsonschema]

type t = {
  name : string;
  age : int;
  email : string option;
  home_address : address; [@ref "shared_address"]
  work_address : address; [@ref "shared_address"]
  retreat_address : address; [@ref "shared_address"]
}
[@@deriving jsonschema]

let schema =
  Ppx_deriving_jsonschema_runtime.json_schema
    ~definitions:[("shared_address", address_jsonschema)]
    t_jsonschema
```

Would produce the following schema:
```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$defs": {
    "shared_address": {
      "type": "object",
      "properties": {
        "zip": { "type": "string" },
        "city": { "type": "string" },
        "street": { "type": "string" }
      },
      "required": [ "zip", "city", "street" ]
    }
  },
  "type": "object",
  "properties": {
    "retreat_address": { "$ref": "#/$defs/shared_address" },
    "work_address": { "$ref": "#/$defs/shared_address" },
    "home_address": { "$ref": "#/$defs/shared_address" },
    "email": { "type": "string" },
    "age": { "type": "integer" },
    "name": { "type": "string" }
  },
  "required": [
    "retreat_address", "work_address", "home_address", "age", "name"
  ]
}
```
