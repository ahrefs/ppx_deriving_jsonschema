[@@@ocaml.warning "-37-69"]

let print_schema ?definitions ?id ?title ?description s =
  let s = Ppx_deriving_jsonschema_runtime.json_schema ?definitions ?id ?title ?description s in
  let () = print_endline (Yojson.Basic.pretty_to_string s) in
  ()

module Mod1 = struct
  type m_1 =
    | A
    | B
  [@@deriving jsonschema]

  let%expect_test "m_1" =
    print_schema m_1_jsonschema;
    [%expect
      {|
      {
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "anyOf": [ { "const": "A" }, { "const": "B" } ]
      }
    |}]

  module Mod2 = struct
    type m_2 =
      | C
      | D
    [@@deriving jsonschema]

    let%expect_test "m_2" =
      print_schema m_2_jsonschema;
      [%expect
        {|
        {
          "$schema": "https://json-schema.org/draft/2020-12/schema",
          "anyOf": [ { "const": "C" }, { "const": "D" } ]
        }
      |}]
  end
end

type with_modules = {
  m : Mod1.m_1;
  m2 : Mod1.Mod2.m_2;
}
[@@deriving jsonschema]

let%expect_test "with_modules" =
  print_schema with_modules_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "m2": { "anyOf": [ { "const": "C" }, { "const": "D" } ] },
        "m": { "anyOf": [ { "const": "A" }, { "const": "B" } ] }
      },
      "required": [ "m2", "m" ]
    } |}]

type kind =
  | Success
  | Error
  | Skipped [@name "skipped"]
[@@deriving jsonschema]

let%expect_test "kind" =
  print_schema kind_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        { "const": "Success" }, { "const": "Error" }, { "const": "skipped" }
      ]
    } |}]

type kind_as_array =
  | Success
  | Error
  | Skipped [@name "skipped"]
[@@deriving jsonschema ~variant_as_array]

let%expect_test "kind_as_array" =
  print_schema kind_as_array_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "Success" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "Error" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "skipped" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        }
      ]
    } |}]

type poly_kind =
  [ `Aaa
  | `Bbb
  | `Ccc [@name "ccc"]
  ]
[@@deriving jsonschema]

let%expect_test "poly_kind" =
  print_schema poly_kind_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Aaa" }, { "const": "Bbb" }, { "const": "ccc" } ]
    } |}]

type poly_kind_as_array =
  [ `Aaa
  | `Bbb
  | `Ccc [@name "ccc"]
  ]
[@@deriving jsonschema ~variant_as_array]

let%expect_test "poly_kind_as_array" =
  print_schema poly_kind_as_array_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "Aaa" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "Bbb" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "ccc" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        }
      ]
    } |}]

type poly_kind_with_payload =
  [ `Aaa of int
  | `Bbb
  | `Ccc of string * bool [@name "ccc"]
  ]
[@@deriving jsonschema]

let%expect_test "poly_kind_with_payload" =
  print_schema poly_kind_with_payload_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Aaa" }, { "const": "Bbb" }, { "const": "ccc" } ]
    } |}]

type poly_kind_with_payload_as_array =
  [ `Aaa of int
  | `Bbb
  | `Ccc of string * bool [@name "ccc"]
  ]
[@@deriving jsonschema ~variant_as_array]

let%expect_test "poly_kind_with_payload_as_array" =
  print_schema poly_kind_with_payload_as_array_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "Aaa" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "Bbb" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "ccc" },
            {
              "type": "array",
              "prefixItems": [ { "type": "string" }, { "type": "boolean" } ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    } |}]

type poly_inherit =
  [ `New_one
  | `Second_one of int
  | poly_kind
  ]
[@@deriving jsonschema]

let%expect_test "poly_inherit" =
  print_schema poly_inherit_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        { "const": "New_one" },
        { "const": "Second_one" },
        {
          "anyOf": [ { "const": "Aaa" }, { "const": "Bbb" }, { "const": "ccc" } ]
        }
      ]
    } |}]

type poly_inherit_as_array =
  [ `New_one
  | `Second_one of int
  | poly_kind_as_array
  ]
[@@deriving jsonschema ~variant_as_array]

let%expect_test "poly_inherit_as_array" =
  print_schema poly_inherit_as_array_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "New_one" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "Second_one" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "Aaa" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "Bbb" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "ccc" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            }
          ]
        }
      ]
    } |}]

type event = {
  date : float;
  kind_f : kind;
  comment : string;
  opt : int option; [@key "opt_int"]
  a : float array;
  l : string list;
  t : [ `Foo | `Bar | `Baz ];
  c : char;
  bunch_of_bytes : bytes;
  string_ref : string ref;
  unit : unit;
  native_int : nativeint;
}
[@@deriving jsonschema]

let%expect_test "event" =
  print_schema event_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "native_int": { "type": "integer" },
        "unit": { "type": "null" },
        "string_ref": { "type": "string" },
        "bunch_of_bytes": { "type": "string" },
        "c": { "type": "string", "minLength": 1, "maxLength": 1 },
        "t": {
          "anyOf": [ { "const": "Foo" }, { "const": "Bar" }, { "const": "Baz" } ]
        },
        "l": { "type": "array", "items": { "type": "string" } },
        "a": { "type": "array", "items": { "type": "number" } },
        "opt_int": { "type": "integer" },
        "comment": { "type": "string" },
        "kind_f": {
          "anyOf": [
            { "const": "Success" }, { "const": "Error" }, { "const": "skipped" }
          ]
        },
        "date": { "type": "number" }
      },
      "required": [
        "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l", "a",
        "comment", "kind_f", "date"
      ]
    } |}]

(* type recursive_record = {
     a : int;
     b : recursive_record list;
   }
   [@@deriving jsonschema]

   let () = print_schema recursive_record_jsonschema

   type recursive_variant =
     | A of recursive_variant
     | B
   [@@deriving jsonschema]

   let () = print_schema recursive_variant_jsonschema *)

type events = event list [@@deriving jsonschema]

let%expect_test "events" =
  print_schema events_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "native_int": { "type": "integer" },
          "unit": { "type": "null" },
          "string_ref": { "type": "string" },
          "bunch_of_bytes": { "type": "string" },
          "c": { "type": "string", "minLength": 1, "maxLength": 1 },
          "t": {
            "anyOf": [
              { "const": "Foo" }, { "const": "Bar" }, { "const": "Baz" }
            ]
          },
          "l": { "type": "array", "items": { "type": "string" } },
          "a": { "type": "array", "items": { "type": "number" } },
          "opt_int": { "type": "integer" },
          "comment": { "type": "string" },
          "kind_f": {
            "anyOf": [
              { "const": "Success" },
              { "const": "Error" },
              { "const": "skipped" }
            ]
          },
          "date": { "type": "number" }
        },
        "required": [
          "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l",
          "a", "comment", "kind_f", "date"
        ]
      }
    } |}]

type eventss = event list list [@@deriving jsonschema]

let%expect_test "eventss" =
  print_schema eventss_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "items": {
        "type": "array",
        "items": {
          "type": "object",
          "properties": {
            "native_int": { "type": "integer" },
            "unit": { "type": "null" },
            "string_ref": { "type": "string" },
            "bunch_of_bytes": { "type": "string" },
            "c": { "type": "string", "minLength": 1, "maxLength": 1 },
            "t": {
              "anyOf": [
                { "const": "Foo" }, { "const": "Bar" }, { "const": "Baz" }
              ]
            },
            "l": { "type": "array", "items": { "type": "string" } },
            "a": { "type": "array", "items": { "type": "number" } },
            "opt_int": { "type": "integer" },
            "comment": { "type": "string" },
            "kind_f": {
              "anyOf": [
                { "const": "Success" },
                { "const": "Error" },
                { "const": "skipped" }
              ]
            },
            "date": { "type": "number" }
          },
          "required": [
            "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l",
            "a", "comment", "kind_f", "date"
          ]
        }
      }
    } |}]

type event_comment = event * string [@@deriving jsonschema]

let%expect_test "event_comment" =
  print_schema event_comment_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "prefixItems": [
        {
          "type": "object",
          "properties": {
            "native_int": { "type": "integer" },
            "unit": { "type": "null" },
            "string_ref": { "type": "string" },
            "bunch_of_bytes": { "type": "string" },
            "c": { "type": "string", "minLength": 1, "maxLength": 1 },
            "t": {
              "anyOf": [
                { "const": "Foo" }, { "const": "Bar" }, { "const": "Baz" }
              ]
            },
            "l": { "type": "array", "items": { "type": "string" } },
            "a": { "type": "array", "items": { "type": "number" } },
            "opt_int": { "type": "integer" },
            "comment": { "type": "string" },
            "kind_f": {
              "anyOf": [
                { "const": "Success" },
                { "const": "Error" },
                { "const": "skipped" }
              ]
            },
            "date": { "type": "number" }
          },
          "required": [
            "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l",
            "a", "comment", "kind_f", "date"
          ]
        },
        { "type": "string" }
      ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    } |}]

type event_comments' = event_comment list [@@deriving jsonschema]

let%expect_test "event_comments'" =
  print_schema event_comments'_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "items": {
        "type": "array",
        "prefixItems": [
          {
            "type": "object",
            "properties": {
              "native_int": { "type": "integer" },
              "unit": { "type": "null" },
              "string_ref": { "type": "string" },
              "bunch_of_bytes": { "type": "string" },
              "c": { "type": "string", "minLength": 1, "maxLength": 1 },
              "t": {
                "anyOf": [
                  { "const": "Foo" }, { "const": "Bar" }, { "const": "Baz" }
                ]
              },
              "l": { "type": "array", "items": { "type": "string" } },
              "a": { "type": "array", "items": { "type": "number" } },
              "opt_int": { "type": "integer" },
              "comment": { "type": "string" },
              "kind_f": {
                "anyOf": [
                  { "const": "Success" },
                  { "const": "Error" },
                  { "const": "skipped" }
                ]
              },
              "date": { "type": "number" }
            },
            "required": [
              "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t",
              "l", "a", "comment", "kind_f", "date"
            ]
          },
          { "type": "string" }
        ],
        "unevaluatedItems": false,
        "minItems": 2,
        "maxItems": 2
      }
    } |}]

type event_n = (event * int) list [@@deriving jsonschema]

let%expect_test "event_n" =
  print_schema event_n_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "items": {
        "type": "array",
        "prefixItems": [
          {
            "type": "object",
            "properties": {
              "native_int": { "type": "integer" },
              "unit": { "type": "null" },
              "string_ref": { "type": "string" },
              "bunch_of_bytes": { "type": "string" },
              "c": { "type": "string", "minLength": 1, "maxLength": 1 },
              "t": {
                "anyOf": [
                  { "const": "Foo" }, { "const": "Bar" }, { "const": "Baz" }
                ]
              },
              "l": { "type": "array", "items": { "type": "string" } },
              "a": { "type": "array", "items": { "type": "number" } },
              "opt_int": { "type": "integer" },
              "comment": { "type": "string" },
              "kind_f": {
                "anyOf": [
                  { "const": "Success" },
                  { "const": "Error" },
                  { "const": "skipped" }
                ]
              },
              "date": { "type": "number" }
            },
            "required": [
              "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t",
              "l", "a", "comment", "kind_f", "date"
            ]
          },
          { "type": "integer" }
        ],
        "unevaluatedItems": false,
        "minItems": 2,
        "maxItems": 2
      }
    } |}]

type events_array = events array [@@deriving jsonschema]

let%expect_test "events_array" =
  print_schema events_array_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "items": {
        "type": "array",
        "items": {
          "type": "object",
          "properties": {
            "native_int": { "type": "integer" },
            "unit": { "type": "null" },
            "string_ref": { "type": "string" },
            "bunch_of_bytes": { "type": "string" },
            "c": { "type": "string", "minLength": 1, "maxLength": 1 },
            "t": {
              "anyOf": [
                { "const": "Foo" }, { "const": "Bar" }, { "const": "Baz" }
              ]
            },
            "l": { "type": "array", "items": { "type": "string" } },
            "a": { "type": "array", "items": { "type": "number" } },
            "opt_int": { "type": "integer" },
            "comment": { "type": "string" },
            "kind_f": {
              "anyOf": [
                { "const": "Success" },
                { "const": "Error" },
                { "const": "skipped" }
              ]
            },
            "date": { "type": "number" }
          },
          "required": [
            "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l",
            "a", "comment", "kind_f", "date"
          ]
        }
      }
    } |}]

type numbers = int list [@@deriving jsonschema]

let%expect_test "numbers" =
  print_schema numbers_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "items": { "type": "integer" }
    } |}]

type opt = int option [@@deriving jsonschema]

let%expect_test "opt" =
  print_schema opt_jsonschema;
  [%expect {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "integer"
    } |}]

type using_m = { m : Mod1.m_1 } [@@deriving jsonschema]

let%expect_test "using_m" =
  print_schema using_m_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": { "m": { "anyOf": [ { "const": "A" }, { "const": "B" } ] } },
      "required": [ "m" ]
    } |}]

type 'param2 poly2 = C of 'param2 [@@deriving jsonschema]

let%expect_test "poly2" =
  print_schema poly2_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "C" } ]
    } |}]

type tuple_with_variant = int * [ `A | `B [@name "second_cstr"] ] [@@deriving jsonschema]

let%expect_test "tuple_with_variant" =
  print_schema tuple_with_variant_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "prefixItems": [
        { "type": "integer" },
        { "anyOf": [ { "const": "A" }, { "const": "second_cstr" } ] }
      ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    } |}]

type player_scores = {
  player : string;
  scores : numbers; [@ref "numbers"] [@key "scores_ref"]
}
[@@deriving jsonschema]

let%expect_test "player_scores" =
  print_schema ~id:"https://ahrefs.com/schemas/player_scores" ~title:"Player scores"
    ~description:"Object representing player scores"
    ~definitions:[ "numbers", numbers_jsonschema ]
    player_scores_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "https://ahrefs.com/schemas/player_scores",
      "title": "Player scores",
      "description": "Object representing player scores",
      "$defs": { "numbers": { "type": "array", "items": { "type": "integer" } } },
      "type": "object",
      "properties": {
        "scores_ref": { "$ref": "#/$defs/numbers" },
        "player": { "type": "string" }
      },
      "required": [ "scores_ref", "player" ]
    } |}]

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
  address : address;
}
[@@deriving jsonschema]

let%expect_test "t" =
  print_schema t_jsonschema;
  [%expect
    {|
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
    } |}]

type tt = {
  name : string;
  age : int;
  email : string option;
  home_address : address; [@ref "shared_address"]
  work_address : address; [@ref "shared_address"]
  retreat_address : address; [@ref "shared_address"]
}
[@@deriving jsonschema]

let%expect_test "tt" =
  print_schema ~definitions:[ "shared_address", address_jsonschema ] tt_jsonschema;
  [%expect
    {|
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
    } |}]

type c = char [@@deriving jsonschema]

let%expect_test "c" =
  print_schema c_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "string",
      "minLength": 1,
      "maxLength": 1
    } |}]

type variant_inline_record =
  | A of { a : int }
  | B of { b : string }
[@@deriving jsonschema ~variant_as_array]

let%expect_test "variant_inline_record" =
  print_schema variant_inline_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [
            { "const": "A" },
            {
              "type": "object",
              "properties": { "a": { "type": "integer" } },
              "required": [ "a" ]
            }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "B" },
            {
              "type": "object",
              "properties": { "b": { "type": "string" } },
              "required": [ "b" ]
            }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    } |}]

type variant_with_payload =
  | A of int
  | B
  | C of int * string
  | D of (int * string * bool)
[@@deriving jsonschema ~variant_as_array]

let%expect_test "variant_with_payload" =
  print_schema variant_with_payload_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "A" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "B" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "C" }, { "type": "integer" }, { "type": "string" }
          ],
          "unevaluatedItems": false,
          "minItems": 3,
          "maxItems": 3
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "D" },
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
    } |}]

type t1 =
  | Typ
  | Class of string
[@@deriving jsonschema]

let%expect_test "t1" =
  print_schema t1_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Typ" }, { "const": "Class" } ]
    } |}]

type t2 =
  | Typ
  | Class of string
[@@deriving jsonschema ~variant_as_array]

let%expect_test "t2" =
  print_schema t2_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
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
    } |}]

type t3 =
  | Typ [@name "type"]
  | Class of string [@name "class"]
[@@deriving jsonschema]

let%expect_test "t3" =
  print_schema t3_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "type" }, { "const": "class" } ]
    } |}]

type t4 = int * string [@@deriving jsonschema]

let%expect_test "t4" =
  print_schema t4_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "prefixItems": [ { "type": "integer" }, { "type": "string" } ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    } |}]
