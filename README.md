# ppx_deriving_jsonschema

`ppx_deriving_jsonschema` is a PPX syntax extension that generates JSON schema from OCaml types.

The conversion aims to be compatible with the existing json derivers.

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

Tuples are converted to `{ "type": "array", "items": [...] }`.

#### Variants and polymorphic variants

Variants are converted to `{ "type": "string", "enum": [...] }`.

if the JSON variant names differ from OCaml conventions, users can specify the corresponding JSON string explicitly using `[@name "constr"]`, for example:

```ocaml
type t =
| Typ   [@name "type"]
| Class [@name "class"]
[@@deriving jsonschema]
```

#### Records

Records are converted to `{ "type": "object", "properties": {...}, "required": [...] }`.

The fields of type `option` are not included in the `required` list.

When the JSON object keys differ from the ocaml field names, users can specify the corresponding JSON key implicitly using `[@key "field"]`, for example:

```ocaml
type t = {
  typ    : float [@key "type"];
  class_ : float [@key "CLASS"];
}
[@@deriving jsonschema]
```

#### References

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
