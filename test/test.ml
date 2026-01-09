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
        "anyOf": [
          {
            "type": "array",
            "prefixItems": [ { "const": "A" } ],
            "unevaluatedItems": false,
            "minItems": 1,
            "maxItems": 1
          },
          {
            "type": "array",
            "prefixItems": [ { "const": "B" } ],
            "unevaluatedItems": false,
            "minItems": 1,
            "maxItems": 1
          }
        ]
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
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "C" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "D" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            }
          ]
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
        "m2": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "C" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "D" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            }
          ]
        },
        "m": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "A" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "B" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            }
          ]
        }
      },
      "required": [ "m2", "m" ],
      "additionalProperties": false
    }
    |}]

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
    }
    |}]

type kind_as_string =
  | Success
  | Error
  | Skipped [@name "skipped"]
[@@deriving jsonschema ~variant_as_string]

let%expect_test "kind_as_string" =
  print_schema kind_as_string_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        { "const": "Success" }, { "const": "Error" }, { "const": "skipped" }
      ]
    }
    |}]

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
    |}]

type poly_kind_as_string =
  [ `Aaa
  | `Bbb
  | `Ccc [@name "ccc"]
  ]
[@@deriving jsonschema ~variant_as_string]

let%expect_test "poly_kind_as_string" =
  print_schema poly_kind_as_string_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Aaa" }, { "const": "Bbb" }, { "const": "ccc" } ]
    }
    |}]

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
            { "const": "ccc" }, { "type": "string" }, { "type": "boolean" }
          ],
          "unevaluatedItems": false,
          "minItems": 3,
          "maxItems": 3
        }
      ]
    }
    |}]

type poly_kind_with_payload_as_string =
  [ `Aaa of int
  | `Bbb
  | `Ccc of string * bool [@name "ccc"]
  ]
[@@deriving jsonschema ~variant_as_string]

let%expect_test "poly_kind_with_payload_as_string" =
  print_schema poly_kind_with_payload_as_string_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Aaa" }, { "const": "Bbb" }, { "const": "ccc" } ]
    }
    |}]

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
    }
    |}]

type poly_inherit_as_string =
  [ `New_one
  | `Second_one of int
  | poly_kind_as_string
  ]
[@@deriving jsonschema ~variant_as_string]

let%expect_test "poly_inherit_as_string" =
  print_schema poly_inherit_as_string_jsonschema;
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
    }
    |}]

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
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "Foo" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "Bar" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "Baz" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            }
          ]
        },
        "l": { "type": "array", "items": { "type": "string" } },
        "a": { "type": "array", "items": { "type": "number" } },
        "opt_int": { "type": "integer" },
        "comment": { "type": "string" },
        "kind_f": {
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
        },
        "date": { "type": "number" }
      },
      "required": [
        "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l", "a",
        "comment", "kind_f", "date"
      ],
      "additionalProperties": false
    }
    |}]

type recursive_record = {
  a : int;
  b : recursive_record list;
}
[@@deriving jsonschema]

let%expect_test "recursive_record" =
  print_schema recursive_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "recursive_record": {
          "type": "object",
          "properties": {
            "b": {
              "type": "array",
              "items": { "$ref": "#/$defs/recursive_record" }
            },
            "a": { "type": "integer" }
          },
          "required": [ "b", "a" ],
          "additionalProperties": false
        }
      },
      "$ref": "#/$defs/recursive_record"
    }
    |}]

type recursive_variant =
  | A of recursive_variant
  | B
[@@deriving jsonschema]

let%expect_test "recursive_variant" =
  print_schema recursive_variant_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "recursive_variant": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [
                { "const": "A" }, { "$ref": "#/$defs/recursive_variant" }
              ],
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
            }
          ]
        }
      },
      "$ref": "#/$defs/recursive_variant"
    }
    |}]

(* Test for tree-like recursive structure *)
type tree =
  | Leaf
  | Node of {
      value : int;
      left : tree;
      right : tree;
    }
[@@deriving jsonschema]

let%expect_test "tree" =
  print_schema tree_jsonschema;
  [%expect
    {|
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
    |}]

(* Non-recursive types should NOT have $defs wrapper *)
type non_recursive = {
  x : int;
  y : string;
}
[@@deriving jsonschema]

let%expect_test "non_recursive" =
  print_schema non_recursive_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": { "y": { "type": "string" }, "x": { "type": "integer" } },
      "required": [ "y", "x" ],
      "additionalProperties": false
    }
    |}]

(* Mutually recursive types *)
type foo = { bar : bar option }
and bar = { foo : foo option }
[@@deriving jsonschema]

let%expect_test "mutually_recursive_foo" =
  print_schema foo_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "foo": {
          "type": "object",
          "properties": { "bar": { "$ref": "#/$defs/bar" } },
          "required": [],
          "additionalProperties": false
        },
        "bar": {
          "type": "object",
          "properties": { "foo": { "$ref": "#/$defs/foo" } },
          "required": [],
          "additionalProperties": false
        }
      },
      "$ref": "#/$defs/foo"
    }
    |}]

let%expect_test "mutually_recursive_bar" =
  print_schema bar_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$ref": "#/$defs/bar"
    }
    |}]

(* More complex mutually recursive: expression/statement pattern *)
type expr =
  | Literal of int
  | Binary of expr * expr
  | Block of stmt list

and stmt =
  | ExprStmt of expr
  | IfStmt of { cond : expr; then_ : stmt; else_ : stmt option }
[@@deriving jsonschema]

let%expect_test "mutually_recursive_expr" =
  print_schema expr_jsonschema;
  [%expect
    {|
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
    |}]

(* Non-recursive mutually defined types should NOT get $defs wrapper *)
type alpha = { x : int }
and beta = { y : string }
[@@deriving jsonschema]

let%expect_test "non_recursive_mutual_alpha" =
  print_schema alpha_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": { "x": { "type": "integer" } },
      "required": [ "x" ],
      "additionalProperties": false
    }
    |}]

let%expect_test "non_recursive_mutual_beta" =
  print_schema beta_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": { "y": { "type": "string" } },
      "required": [ "y" ],
      "additionalProperties": false
    }
    |}]

(* Three mutually recursive types *)
type node_a = { b : node_b option; c : node_c option }
and node_b = { a : node_a option; c : node_c option }
and node_c = { a : node_a option; b : node_b option }
[@@deriving jsonschema]

let%expect_test "three_way_mutual_recursion" =
  print_schema node_a_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "node_a": {
          "type": "object",
          "properties": {
            "c": { "$ref": "#/$defs/node_c" },
            "b": { "$ref": "#/$defs/node_b" }
          },
          "required": [],
          "additionalProperties": false
        },
        "node_b": {
          "type": "object",
          "properties": {
            "c": { "$ref": "#/$defs/node_c" },
            "a": { "$ref": "#/$defs/node_a" }
          },
          "required": [],
          "additionalProperties": false
        },
        "node_c": {
          "type": "object",
          "properties": {
            "b": { "$ref": "#/$defs/node_b" },
            "a": { "$ref": "#/$defs/node_a" }
          },
          "required": [],
          "additionalProperties": false
        }
      },
      "$ref": "#/$defs/node_a"
    }
    |}]

(* Recursive type through tuple *)
type recursive_tuple =
  | Leaf of int
  | Branch of (recursive_tuple * recursive_tuple)
[@@deriving jsonschema]

let%expect_test "recursive_tuple" =
  print_schema recursive_tuple_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "recursive_tuple": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "Leaf" }, { "type": "integer" } ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            },
            {
              "type": "array",
              "prefixItems": [
                { "const": "Branch" },
                {
                  "type": "array",
                  "prefixItems": [
                    { "$ref": "#/$defs/recursive_tuple" },
                    { "$ref": "#/$defs/recursive_tuple" }
                  ],
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
        }
      },
      "$ref": "#/$defs/recursive_tuple"
    }
    |}]

(* Recursive abstract type alias *)
type int_tree = tree [@@deriving jsonschema]

let%expect_test "recursive_abstract_alias" =
  print_schema int_tree_jsonschema;
  [%expect
    {|
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
    |}]

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
              {
                "type": "array",
                "prefixItems": [ { "const": "Foo" } ],
                "unevaluatedItems": false,
                "minItems": 1,
                "maxItems": 1
              },
              {
                "type": "array",
                "prefixItems": [ { "const": "Bar" } ],
                "unevaluatedItems": false,
                "minItems": 1,
                "maxItems": 1
              },
              {
                "type": "array",
                "prefixItems": [ { "const": "Baz" } ],
                "unevaluatedItems": false,
                "minItems": 1,
                "maxItems": 1
              }
            ]
          },
          "l": { "type": "array", "items": { "type": "string" } },
          "a": { "type": "array", "items": { "type": "number" } },
          "opt_int": { "type": "integer" },
          "comment": { "type": "string" },
          "kind_f": {
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
          },
          "date": { "type": "number" }
        },
        "required": [
          "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l",
          "a", "comment", "kind_f", "date"
        ],
        "additionalProperties": false
      }
    }
    |}]

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
                {
                  "type": "array",
                  "prefixItems": [ { "const": "Foo" } ],
                  "unevaluatedItems": false,
                  "minItems": 1,
                  "maxItems": 1
                },
                {
                  "type": "array",
                  "prefixItems": [ { "const": "Bar" } ],
                  "unevaluatedItems": false,
                  "minItems": 1,
                  "maxItems": 1
                },
                {
                  "type": "array",
                  "prefixItems": [ { "const": "Baz" } ],
                  "unevaluatedItems": false,
                  "minItems": 1,
                  "maxItems": 1
                }
              ]
            },
            "l": { "type": "array", "items": { "type": "string" } },
            "a": { "type": "array", "items": { "type": "number" } },
            "opt_int": { "type": "integer" },
            "comment": { "type": "string" },
            "kind_f": {
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
            },
            "date": { "type": "number" }
          },
          "required": [
            "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l",
            "a", "comment", "kind_f", "date"
          ],
          "additionalProperties": false
        }
      }
    }
    |}]

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
                {
                  "type": "array",
                  "prefixItems": [ { "const": "Foo" } ],
                  "unevaluatedItems": false,
                  "minItems": 1,
                  "maxItems": 1
                },
                {
                  "type": "array",
                  "prefixItems": [ { "const": "Bar" } ],
                  "unevaluatedItems": false,
                  "minItems": 1,
                  "maxItems": 1
                },
                {
                  "type": "array",
                  "prefixItems": [ { "const": "Baz" } ],
                  "unevaluatedItems": false,
                  "minItems": 1,
                  "maxItems": 1
                }
              ]
            },
            "l": { "type": "array", "items": { "type": "string" } },
            "a": { "type": "array", "items": { "type": "number" } },
            "opt_int": { "type": "integer" },
            "comment": { "type": "string" },
            "kind_f": {
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
            },
            "date": { "type": "number" }
          },
          "required": [
            "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l",
            "a", "comment", "kind_f", "date"
          ],
          "additionalProperties": false
        },
        { "type": "string" }
      ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    }
    |}]

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
                  {
                    "type": "array",
                    "prefixItems": [ { "const": "Foo" } ],
                    "unevaluatedItems": false,
                    "minItems": 1,
                    "maxItems": 1
                  },
                  {
                    "type": "array",
                    "prefixItems": [ { "const": "Bar" } ],
                    "unevaluatedItems": false,
                    "minItems": 1,
                    "maxItems": 1
                  },
                  {
                    "type": "array",
                    "prefixItems": [ { "const": "Baz" } ],
                    "unevaluatedItems": false,
                    "minItems": 1,
                    "maxItems": 1
                  }
                ]
              },
              "l": { "type": "array", "items": { "type": "string" } },
              "a": { "type": "array", "items": { "type": "number" } },
              "opt_int": { "type": "integer" },
              "comment": { "type": "string" },
              "kind_f": {
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
              },
              "date": { "type": "number" }
            },
            "required": [
              "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t",
              "l", "a", "comment", "kind_f", "date"
            ],
            "additionalProperties": false
          },
          { "type": "string" }
        ],
        "unevaluatedItems": false,
        "minItems": 2,
        "maxItems": 2
      }
    }
    |}]

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
                  {
                    "type": "array",
                    "prefixItems": [ { "const": "Foo" } ],
                    "unevaluatedItems": false,
                    "minItems": 1,
                    "maxItems": 1
                  },
                  {
                    "type": "array",
                    "prefixItems": [ { "const": "Bar" } ],
                    "unevaluatedItems": false,
                    "minItems": 1,
                    "maxItems": 1
                  },
                  {
                    "type": "array",
                    "prefixItems": [ { "const": "Baz" } ],
                    "unevaluatedItems": false,
                    "minItems": 1,
                    "maxItems": 1
                  }
                ]
              },
              "l": { "type": "array", "items": { "type": "string" } },
              "a": { "type": "array", "items": { "type": "number" } },
              "opt_int": { "type": "integer" },
              "comment": { "type": "string" },
              "kind_f": {
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
              },
              "date": { "type": "number" }
            },
            "required": [
              "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t",
              "l", "a", "comment", "kind_f", "date"
            ],
            "additionalProperties": false
          },
          { "type": "integer" }
        ],
        "unevaluatedItems": false,
        "minItems": 2,
        "maxItems": 2
      }
    }
    |}]

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
                {
                  "type": "array",
                  "prefixItems": [ { "const": "Foo" } ],
                  "unevaluatedItems": false,
                  "minItems": 1,
                  "maxItems": 1
                },
                {
                  "type": "array",
                  "prefixItems": [ { "const": "Bar" } ],
                  "unevaluatedItems": false,
                  "minItems": 1,
                  "maxItems": 1
                },
                {
                  "type": "array",
                  "prefixItems": [ { "const": "Baz" } ],
                  "unevaluatedItems": false,
                  "minItems": 1,
                  "maxItems": 1
                }
              ]
            },
            "l": { "type": "array", "items": { "type": "string" } },
            "a": { "type": "array", "items": { "type": "number" } },
            "opt_int": { "type": "integer" },
            "comment": { "type": "string" },
            "kind_f": {
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
            },
            "date": { "type": "number" }
          },
          "required": [
            "native_int", "unit", "string_ref", "bunch_of_bytes", "c", "t", "l",
            "a", "comment", "kind_f", "date"
          ],
          "additionalProperties": false
        }
      }
    }
    |}]

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
      "properties": {
        "m": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "A" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "B" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            }
          ]
        }
      },
      "required": [ "m" ],
      "additionalProperties": false
    }
    |}]

(* This type won't work without ~variant_as_string because of the polymorphic payload. *)
type 'param2 poly2 = C of 'param2 [@@deriving jsonschema ~variant_as_string]

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
        {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "A" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "second_cstr" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            }
          ]
        }
      ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    }
    |}]

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
      "required": [ "scores_ref", "player" ],
      "additionalProperties": false
    }
    |}]

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
          "required": [ "zip", "city", "street" ],
          "additionalProperties": false
        },
        "email": { "type": "string" },
        "age": { "type": "integer" },
        "name": { "type": "string" }
      },
      "required": [ "address", "age", "name" ],
      "additionalProperties": false
    }
    |}]

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
          "required": [ "zip", "city", "street" ],
          "additionalProperties": false
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
      ],
      "additionalProperties": false
    }
    |}]

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
[@@deriving jsonschema ~variant_as_string]

let%expect_test "variant_inline_record" =
  print_schema variant_inline_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "A" }, { "const": "B" } ]
    }
    |}]

type inline_record_with_extra_fields =
  | User of {
      name : string;
      email : string;
    } [@jsonschema.allow_extra_fields]
  | Guest of { ip : string }
[@@deriving jsonschema]

let%expect_test "inline_record_with_extra_fields" =
  print_schema inline_record_with_extra_fields_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
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
    |}]

type variant_with_payload =
  | A of int
  | B
  | C of int * string
  | D of (int * string * bool)
[@@deriving jsonschema ~variant_as_string]

let%expect_test "variant_with_payload" =
  print_schema variant_with_payload_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        { "const": "A" }, { "const": "B" }, { "const": "C" }, { "const": "D" }
      ]
    }
    |}]

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
    |}]

type t2 =
  | Typ
  | Class of string
[@@deriving jsonschema ~variant_as_string]

let%expect_test "t2" =
  print_schema t2_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Typ" }, { "const": "Class" } ]
    }
    |}]

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
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "type" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "class" }, { "type": "string" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
    |}]

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

type t5 = [ `A of int * string * bool ] [@@deriving jsonschema]

let%expect_test "t5" =
  print_schema t5_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
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
    |}]

type t6 = [ `A of (int * string * bool) * float ] [@@deriving jsonschema]

let%expect_test "t6" =
  print_schema t6_jsonschema;
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
              "type": "array",
              "prefixItems": [
                { "type": "integer" },
                { "type": "string" },
                { "type": "boolean" }
              ],
              "unevaluatedItems": false,
              "minItems": 3,
              "maxItems": 3
            },
            { "type": "number" }
          ],
          "unevaluatedItems": false,
          "minItems": 3,
          "maxItems": 3
        }
      ]
    }
    |}]

type t7 = A of int * string * bool [@@deriving jsonschema]

let%expect_test "t7" =
  print_schema t7_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
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
    |}]

type t8 = A of (int * string * bool) [@@deriving jsonschema]

let%expect_test "t8" =
  print_schema t8_jsonschema;
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
    |}]

type t9 = A of (int * string * bool) * float [@@deriving jsonschema]

let%expect_test "t9" =
  print_schema t9_jsonschema;
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
              "type": "array",
              "prefixItems": [
                { "type": "integer" },
                { "type": "string" },
                { "type": "boolean" }
              ],
              "unevaluatedItems": false,
              "minItems": 3,
              "maxItems": 3
            },
            { "type": "number" }
          ],
          "unevaluatedItems": false,
          "minItems": 3,
          "maxItems": 3
        }
      ]
    }
    |}]

type t10 = [ `A of int * string * bool ] [@@deriving jsonschema]

let%expect_test "t10" =
  print_schema t10_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
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
    |}]

type t11 = [ `B of int * string * bool ] [@@deriving jsonschema ~polymorphic_variant_tuple]

let%expect_test "t11" =
  print_schema t11_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
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
    |}]

type obj2 = { x : int } [@@deriving jsonschema] [@@jsonschema.allow_extra_fields]
type obj1 = { obj2 : obj2 } [@@deriving jsonschema]
type nested_obj = { obj1 : obj1 } [@@deriving jsonschema] [@@allow_extra_fields]

let%expect_test "nested_obj" =
  print_schema nested_obj_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "obj1": {
          "type": "object",
          "properties": {
            "obj2": {
              "type": "object",
              "properties": { "x": { "type": "integer" } },
              "required": [ "x" ],
              "additionalProperties": true
            }
          },
          "required": [ "obj2" ],
          "additionalProperties": false
        }
      },
      "required": [ "obj1" ],
      "additionalProperties": true
    }
    |}]

open Melange_json.Primitives

type x_without_extra = { x : int } [@@deriving json, jsonschema] [@@allow_extra_fields]
type x_with_extra = {
  x : int;
  y : int;
}
[@@deriving json, jsonschema] [@@allow_extra_fields]

let%expect_test "extra_fields" =
  let _check_deseralization_ok = { x = 1; y = 1 } |> x_with_extra_to_json |> x_without_extra_of_json in
  print_schema x_without_extra_jsonschema;
  [%expect
    "\n\
    \ {\n\
    \   \"$schema\": \"https://json-schema.org/draft/2020-12/schema\",\n\
    \   \"type\": \"object\",\n\
    \   \"properties\": { \"x\": { \"type\": \"integer\" } },\n\
    \   \"required\": [ \"x\" ],\n\
    \   \"additionalProperties\": true\n\
    \ }\n\
    \ "]
