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

## Usage

To generate jsonschema for a type, add the `[@@deriving jsonschema]` attribute to
the type declaration.

```ocaml
open Ppx_deriving_jsonschema_runtime.Primitives.Melange_json

type t = {
  a: int;
  b: string;
} [@@deriving jsonschema]
```

### Conversion rules

#### Primitives

As we support the ppx to be used with both melange-json and yojson, we provide two primitives modules: `Melange_json` and `Yojson`.

##### Melange_json

<table>
<thead>
<tr><th>OCaml type</th><th>JSON schema</th></tr>
</thead>
<tbody>
<tr>
<td><code>char</code></td>
<td>

```json
{ "type": "string", "minLength": 1, "maxLength": 1 }
```

</tr>
<tr>
<td><code>int</code></td>
<td>

```json
{ "type": "integer" }
```

</td>
</tr>
<tr>
<td><code>int64</code></td>
<td>

```json
{ "type": "string", "description": "int64 is represented as a string" }
```

</td>
</tr>
<tr>
<td><code>float</code></td>
<td>

```json
{ "type": "number" }
```

</td>
</tr>
<tr>
<td><code>bool</code></td>
<td>

```json
{ "type": "boolean" }
```

</td>
</tr>
<tr>
<td><code>string</code></td>
<td>

```json
{ "type": "string" }
```

</td>
</tr>
<tr>
<td><code>list</code>, <code>array</code></td>
<td>

```json
{ "type": "array", "items": { "type": "..." } }
```

</td>
</tr>
<tr>
<td><code>'a option</code></td>
<td>

```json
{ "type": ["...", "null"] }
```

</td>
</tr>
<tr>
<td><code>unit</code></td>
<td>

```json
{ "type": "null" }
```

</td>
</tr>
<tr>
<td><code>result('a, 'b)</code></td>
<td>

```json
{
  "anyOf": [
    {
      "type": "array",
      "prefixItems": [ { "const": "Error" }, { "type": "..." } ],
      "unevaluatedItems": false,
      "minItems": 1
    },
    {
      "type": "array",
      "prefixItems": [ { "const": "Ok" }, { "type": "..." } ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    }
  ]
}
```

</td>
</tr>
</tbody>
</table>


##### Yojson

<table>
<thead>
<tr><th>OCaml type</th><th>JSON schema</th></tr>
</thead>
<tbody>
<tr>
<td><code>char</code></td>
<td>

```json
{ "type": "string", "minLength": 1, "maxLength": 1 }
```

</td>
</tr>
<tr>
<td><code>int</code>, <code>int64</code></td>
<td>

```json
{ "type": "number" }
```

</td>
</tr>
<tr>
<td><code>float</code></td>
<td>

```json
{ "type": "number" }
```

</td>
</tr>
<tr>
<td><code>bool</code></td>
<td>

```json
{ "type": "boolean" }
```

</td>
</tr>
<tr>
<td><code>string</code></td>
<td>

```json
{ "type": "string" }
```

</td>
</tr>
<tr>
<td><code>list</code>, <code>array</code></td>
<td>

```json
{ "type": "array", "items": { "type": "..." } }
```

</td>
</tr>
<tr>
<td><code>'a option</code></td>
<td>

```json
{ "type": ["...", "null"] }
```

</td>
</tr>
<tr>
<td><code>unit</code></td>
<td>

```json
{ "type": "null" }
```

</td>
</tr>
</tbody>
</table>

#### Ref

Type `'a ref` is treated as `'a`.

```ocaml
type t = {
  name : string ref;
} [@@deriving jsonschema]
```

```json
{ "type": "string" }
```

#### Option

Option types are converted to `{ "type": ["...", "null"] }` and added to the `required` list.

```ocaml
type t = {
  name : string option;
} [@@deriving jsonschema]
```

```json
{ 
  "type": "object", 
  "properties": { 
    "name": { 
      "type": [ "string", "null" ] 
    },
  },
  "required": [ "name" ], 
  "additionalProperties": false 
}
```

To make a field optional, use the `[@@jsonschema.option]` attribute:

```ocaml
type t = {
  name : string option; [@jsonschema.option]
} [@@deriving jsonschema]
```

```json
{ 
  "type": "object", 
  "properties": { 
    "name": { 
      "type": [ "string", "null" ] 
    },
  },
  "required": [], 
  "additionalProperties": false 
}
```
#### Result

`('ok, 'err) result` is converted to an `anyOf` with two array variants, matching the standard variant encoding:

```ocaml
type result_value = (int, string) result [@@deriving jsonschema]
```

```json
{
  "anyOf": [
    {
      "type": "array",
      "prefixItems": [ { "const": "Error" }, { "type": "integer" } ],
      "unevaluatedItems": false,
      "minItems": 1
    },
    {
      "type": "array",
      "prefixItems": [ { "const": "Ok" }, { "type": "string" } ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    }
  ]
}
```

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

By default, additionalProperties are not allowed in objects. To allow additionalProperties, use the `allow_extra_fields` attribute:

```ocaml
type company = {
  name : string;
  employees : int;
}
[@@deriving jsonschema]
[@@jsonschema.allow_extra_fields]
```

This annotation will generate a schema with `"additionalProperties": true`, allowing for additional fields not defined in the record:

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

#### Inline Records in Variants

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

#### Recursive Types

Recursive types are automatically detected and handled using JSON Schema's `$defs` and `$ref` mechanism.

##### Self-referential types

```ocaml
type tree =
  | Leaf
  | Node of { value : int; left : tree; right : tree }
[@@deriving jsonschema]
```

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$defs": {
    "tree": {
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "Leaf" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "Node" },
            {
              "type": "object",
              "properties": {
                "right": { "$ref": "#/$defs/tree" },
                "left": { "$ref": "#/$defs/tree" },
                "value": { "type": "integer" }
              },
              "required": [ "right", "left", "value" ],
              "additionalProperties": false
            }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
  },
  "$ref": "#/$defs/tree"
}
```

##### Mutually recursive types

Mutually recursive types (defined with `and`) are also supported:

```ocaml
type expr =
  | Literal of int
  | Binary of expr * expr
  | Block of stmt list

and stmt =
  | ExprStmt of expr
  | IfStmt of { cond : expr; then_ : stmt; else_ : stmt option }
[@@deriving jsonschema]
```

This generates `expr_jsonschema` containing all definitions in `$defs`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$defs": {
    "expr": {
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "Literal" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "Binary" },
            { "$ref": "#/$defs/expr" },
            { "$ref": "#/$defs/expr" }
          ],
          "unevaluatedItems": false,
          "minItems": 3,
          "maxItems": 3
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "Block" },
            { "type": "array", "items": { "$ref": "#/$defs/stmt" } }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    },
    "stmt": {
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [
            { "const": "ExprStmt" }, { "$ref": "#/$defs/expr" }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "IfStmt" },
            {
              "type": "object",
              "properties": {
                "else_": { "$ref": "#/$defs/stmt" },
                "then_": { "$ref": "#/$defs/stmt" },
                "cond": { "$ref": "#/$defs/expr" }
              },
              "required": [ "then_", "cond" ],
              "additionalProperties": false
            }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
  },
  "$ref": "#/$defs/expr"
}
```

The secondary type `stmt_jsonschema` is also a self-contained schema, with the same `$defs` but `$ref` pointing to its own type:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$defs": {
    "expr": { ... },
    "stmt": { ... }
  },
  "$ref": "#/$defs/stmt"
}
```


### Annotations

#### `[@@jsonschema.description]` ([REF](https://www.learnjsonschema.com/2020-12/meta-data/description/))

Add a description to a type or a field. It can be used on a type, a field, a variant constructor, or directly on a core type (e.g. a variant payload).

```ocaml
type t = {
  name : string [@jsonschema.description "The user's full name"];
} [@@deriving jsonschema]
```

```json
{
  "type": "object",
  "properties": {
    "name": { "description": "The user's full name", "type": "string" }
  },
  "required": [ "name" ],
  "additionalProperties": false
}
```

When the `~ocaml_doc` flag is passed to the deriver, an OCaml doc comment (`(** ... *)`, stored in the AST as an `ocaml.doc` attribute) is used as the description when `[@jsonschema.description]` is not provided. The explicit attribute takes precedence. Without the flag, doc comments are ignored.

```ocaml
type t = {
  name : string;  (** The user's full name *)
} [@@deriving jsonschema ~ocaml_doc]
(** A user object *)
```

```json
{
  "description": "A user object",
  "type": "object",
  "properties": {
    "name": { "description": "The user's full name", "type": "string" }
  },
  "required": [ "name" ],
  "additionalProperties": false
}
```

#### `[@@jsonschema.format]` ([REF](https://www.learnjsonschema.com/2020-12/format-annotation/))

Add a format annotation to a string-typed field. It can be used on a type, a field, or directly on a core type (e.g. a variant payload). Only applies to `string` and `bytes` types.

```ocaml
type t = {
  name : string [@jsonschema.format "date-time"]; 
} [@@deriving jsonschema]
```

```json
{
  "type": "object",
  "properties": {
    "name": { "format": "date-time", "type": "string" }
  },
  "required": [ "name" ],
  "additionalProperties": false
}
```

#### `[@@jsonschema.maximum]` ([REF](https://www.learnjsonschema.com/2020-12/validation/maximum/))

Add a maximum value to a type or a field. It can be used on a type, a field, or a variant payload. Only applies to numeric types (`int`, `int32`, `nativeint`, `float`).

```ocaml
type t = {
  score : int [@jsonschema.maximum 100];
} [@@deriving jsonschema]
```

```json
{
  "type": "object",
  "properties": {
    "score": { "maximum": 100, "type": "integer" }
  },
  "required": [ "score" ],
  "additionalProperties": false
}
```

#### `[@@jsonschema.minimum]` ([REF](https://www.learnjsonschema.com/2020-12/validation/minimum/))

Add a minimum value to a type or a field. It can be used on a type, a field, or a variant payload. Only applies to numeric types (`int`, `int32`, `nativeint`, `float`).

```ocaml
type t = {
  score : int [@jsonschema.minimum 0];
} [@@deriving jsonschema]
```

```json
{
  "type": "object",
  "properties": {
    "score": { "minimum": 0, "type": "integer" }
  },
  "required": [ "score" ],
  "additionalProperties": false
}
```

#### `[@jsonschema.default]` ([REF](https://www.learnjsonschema.com/2020-12/meta-data/default/))

Set a default value for a record field. Fields with a default are excluded from `required`.

Primitive literals (`int`, `int32`, `nativeint`, `float`, `string`, `bytes`, `bool`) and **their** `option`, `list`, `tuple`, and `array` variants are serialized automatically. 
For non-primitive types (custom variants, records, etc.) a `<type>_to_json` function must be in scope — e.g. via `[@@deriving json]` from melange-json. (`<type> -> Js.Json.t` at melange and `<type> -> Yojson.Basic.t` at native)

```ocaml
type status = Active | Inactive [@@deriving jsonschema]
let status_to_json = function
  | Active -> `String "Active"
  | Inactive -> `String "Inactive"

type t = {
  score    : int option;  [@jsonschema.default 0]
  label    : string;      [@jsonschema.default "unlabelled"]
  is_admin : bool;        [@jsonschema.default false]
  tags     : string list; [@jsonschema.default ["general"]]
  status   : status;      [@jsonschema.default Active]
} [@@deriving jsonschema]
```

```json
{
  "type": "object",
  "properties": {
    "score":    { "default": 0,             "type": [ "integer", "null" ] },
    "label":    { "default": "unlabelled",  "type": "string" },
    "is_admin": { "default": false,         "type": "boolean" },
    "tags":     { "default": [ "general" ], "type": "array", "items": { "type": "string" } },
    "status":   { "default": [ "Active" ],  "anyOf": [ ... ] }
  },
  "required": [],
  "additionalProperties": false
}
```

#### `[@jsonschema.attrs]`

A composite annotation that bundles multiple schema attributes into a single record expression. Supported fields: `description`, `format`, `maximum`, `minimum`. Type-sensitive fields (`format`, `maximum`, `minimum`) are validated against the annotated type.

Can be used on core types, label declarations, and type declarations.

```ocaml
type t = {
  score : int [@jsonschema.attrs { maximum = 100; minimum = 0; description = "Score out of 100" }];
  created_at : string [@jsonschema.attrs { format = "date-time"; description = "Creation timestamp" }];
} [@@deriving jsonschema]
```

```json
{
  "type": "object",
  "properties": {
    "score": { "minimum": 0, "maximum": 100, "description": "Score out of 100", "type": "integer" },
    "created_at": { "format": "date-time", "description": "Creation timestamp", "type": "string" }
  },
  "required": [ "score", "created_at" ],
  "additionalProperties": false
}
```

It can also be applied directly to a core type in a variant payload:

```ocaml
type t =
  | Score of (int [@jsonschema.attrs { maximum = 100; minimum = 0; description = "Percentage" }])
[@@deriving jsonschema]
```