[@@@ocaml.warning "-37-69"]

let print_schema ?definitions ?id ?title ?description s =
  let s = Ppx_deriving_jsonschema_runtime.json_schema ?definitions ?id ?title ?description s in
  let () = print_endline (Yojson.Basic.pretty_to_string s) in
  ()

let string_jsonschema = `Assoc [ "type", `String "string" ]
let int_jsonschema = `Assoc [ "type", `String "integer" ]
let bool_jsonschema = `Assoc [ "type", `String "boolean" ]

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
        "opt_int": { "type": [ "integer", "null" ] },
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
        "opt_int", "comment", "kind_f", "date"
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
and bar = { foo : foo option } [@@deriving jsonschema]

let%expect_test "mutually_recursive_foo" =
  print_schema foo_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "foo": {
          "type": "object",
          "properties": {
            "bar": { "anyOf": [ { "$ref": "#/$defs/bar" }, { "type": "null" } ] }
          },
          "required": [ "bar" ],
          "additionalProperties": false
        },
        "bar": {
          "type": "object",
          "properties": {
            "foo": { "anyOf": [ { "$ref": "#/$defs/foo" }, { "type": "null" } ] }
          },
          "required": [ "foo" ],
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
      "$defs": {
        "foo": {
          "type": "object",
          "properties": {
            "bar": { "anyOf": [ { "$ref": "#/$defs/bar" }, { "type": "null" } ] }
          },
          "required": [ "bar" ],
          "additionalProperties": false
        },
        "bar": {
          "type": "object",
          "properties": {
            "foo": { "anyOf": [ { "$ref": "#/$defs/foo" }, { "type": "null" } ] }
          },
          "required": [ "foo" ],
          "additionalProperties": false
        }
      },
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
  | IfStmt of {
      cond : expr;
      then_ : stmt;
      else_ : stmt option;
    }
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
                    "else_": {
                      "anyOf": [ { "$ref": "#/$defs/stmt" }, { "type": "null" } ]
                    },
                    "then_": { "$ref": "#/$defs/stmt" },
                    "cond": { "$ref": "#/$defs/expr" }
                  },
                  "required": [ "else_", "then_", "cond" ],
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
and beta = { y : string } [@@deriving jsonschema]

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
type node_a = {
  b : node_b option;
  c : node_c option;
}
and node_b = {
  a : node_a option;
  c : node_c option;
}
and node_c = {
  a : node_a option;
  b : node_b option;
}
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
            "c": {
              "anyOf": [ { "$ref": "#/$defs/node_c" }, { "type": "null" } ]
            },
            "b": {
              "anyOf": [ { "$ref": "#/$defs/node_b" }, { "type": "null" } ]
            }
          },
          "required": [ "c", "b" ],
          "additionalProperties": false
        },
        "node_b": {
          "type": "object",
          "properties": {
            "c": {
              "anyOf": [ { "$ref": "#/$defs/node_c" }, { "type": "null" } ]
            },
            "a": {
              "anyOf": [ { "$ref": "#/$defs/node_a" }, { "type": "null" } ]
            }
          },
          "required": [ "c", "a" ],
          "additionalProperties": false
        },
        "node_c": {
          "type": "object",
          "properties": {
            "b": {
              "anyOf": [ { "$ref": "#/$defs/node_b" }, { "type": "null" } ]
            },
            "a": {
              "anyOf": [ { "$ref": "#/$defs/node_a" }, { "type": "null" } ]
            }
          },
          "required": [ "b", "a" ],
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
      "$id": "file://test/test.ml:912",
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
          "opt_int": { "type": [ "integer", "null" ] },
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
          "a", "opt_int", "comment", "kind_f", "date"
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
            "opt_int": { "type": [ "integer", "null" ] },
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
            "a", "opt_int", "comment", "kind_f", "date"
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
            "opt_int": { "type": [ "integer", "null" ] },
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
            "a", "opt_int", "comment", "kind_f", "date"
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
              "opt_int": { "type": [ "integer", "null" ] },
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
              "l", "a", "opt_int", "comment", "kind_f", "date"
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
              "opt_int": { "type": [ "integer", "null" ] },
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
              "l", "a", "opt_int", "comment", "kind_f", "date"
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
            "opt_int": { "type": [ "integer", "null" ] },
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
            "a", "opt_int", "comment", "kind_f", "date"
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
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": [ "integer", "null" ]
    }
    |}]

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
  print_schema (poly2_jsonschema int_jsonschema);
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
        "email": { "type": [ "string", "null" ] },
        "age": { "type": "integer" },
        "name": { "type": "string" }
      },
      "required": [ "address", "email", "age", "name" ],
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
        "email": { "type": [ "string", "null" ] },
        "age": { "type": "integer" },
        "name": { "type": "string" }
      },
      "required": [
        "retreat_address", "work_address", "home_address", "email", "age", "name"
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

type x_without_extra = { x : int } [@@deriving jsonschema] [@@allow_extra_fields]
type x_with_extra = {
  x : int;
  y : int;
}
[@@deriving jsonschema] [@@allow_extra_fields]

let%expect_test "extra_fields" =
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

type 'url generic_link_traffic = {
  title : string option;
  url : 'url;
}
[@@deriving jsonschema]

let%expect_test "parameterized_record" =
  print_schema (generic_link_traffic_jsonschema string_jsonschema);
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "url": { "type": "string" },
        "title": { "type": [ "string", "null" ] }
      },
      "required": [ "url", "title" ],
      "additionalProperties": false
    }
    |}]

type string_link_traffic = string generic_link_traffic [@@deriving jsonschema]

let%expect_test "instantiated_parameterized_record" =
  print_schema string_link_traffic_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "url": { "type": "string" },
        "title": { "type": [ "string", "null" ] }
      },
      "required": [ "url", "title" ],
      "additionalProperties": false
    }
    |}]

type 'a poly_variant =
  | A
  | B of 'a
[@@deriving jsonschema]

let%expect_test "parameterized_variant" =
  print_schema (poly_variant_jsonschema int_jsonschema);
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
          "prefixItems": [ { "const": "B" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    } |}]

type ('a, 'b) multi_param = {
  first : 'a;
  second : 'b;
  label : string;
}
[@@deriving jsonschema]

let%expect_test "multi_param" =
  print_schema (multi_param_jsonschema int_jsonschema bool_jsonschema);
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "label": { "type": "string" },
        "second": { "type": "boolean" },
        "first": { "type": "integer" }
      },
      "required": [ "label", "second", "first" ],
      "additionalProperties": false
    } |}]

type 'a param_list = 'a list [@@deriving jsonschema]

let%expect_test "parameterized_abstract" =
  print_schema (param_list_jsonschema string_jsonschema);
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "items": { "type": "string" }
    } |}]

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b
[@@deriving jsonschema]

let%expect_test "multi_param_variant" =
  print_schema (either_jsonschema int_jsonschema string_jsonschema);
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "Left" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "Right" }, { "type": "string" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
    |}]

type ('a, 'b) either_alias = ('a, 'b) either [@@deriving jsonschema]

let%expect_test "multi_param_abstract" =
  print_schema (either_alias_jsonschema int_jsonschema string_jsonschema);
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "Left" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "Right" }, { "type": "string" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
    |}]

type ('a, 'b) direction =
  | North
  | South
[@@deriving jsonschema ~variant_as_string]

let%expect_test "multi_param_variant_as_string" =
  print_schema (direction_jsonschema int_jsonschema string_jsonschema);
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "North" }, { "const": "South" } ]
    }
    |}]

(* Description attribute tests *)

type tool_params = {
  query : string; [@jsonschema.description "The search query to execute"]
  max_results : int; [@jsonschema.description "Maximum number of results to return"]
}
[@@deriving jsonschema]

let%expect_test "field_description" =
  print_schema tool_params_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "max_results": {
          "description": "Maximum number of results to return",
          "type": "integer"
        },
        "query": {
          "description": "The search query to execute",
          "type": "string"
        }
      },
      "required": [ "max_results", "query" ],
      "additionalProperties": false
    }
    |}]

type described_record = {
  name : string; [@jsonschema.description "The user's full name"]
  age : int option; [@jsonschema.option] [@jsonschema.description "The user's age"]
}
[@@deriving jsonschema] [@@jsonschema.description "A user object"]

let%expect_test "type_and_field_description" =
  print_schema described_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "description": "A user object",
      "type": "object",
      "properties": {
        "age": { "description": "The user's age", "type": [ "integer", "null" ] },
        "name": { "description": "The user's full name", "type": "string" }
      },
      "required": [ "name" ],
      "additionalProperties": false
    }
    |}]

type with_key_and_desc = {
  opt : int option; [@key "opt_int"] [@jsonschema.option] [@jsonschema.description "An optional integer"]
}
[@@deriving jsonschema]

let%expect_test "field_description_with_key" =
  print_schema with_key_and_desc_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "opt_int": {
          "description": "An optional integer",
          "type": [ "integer", "null" ]
        }
      },
      "required": [],
      "additionalProperties": false
    }
    |}]

type described_variant =
  | Plain [@jsonschema.description "No payload"]
  | With_int of int [@jsonschema.description "Single integer tag"]
  | Pair of string * int [@jsonschema.description "String and int"]
  | Description_value of (string[@jsonschema.description "A string value"])
[@@deriving jsonschema]

let%expect_test "variant_constructor_description" =
  print_schema described_variant_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "description": "No payload",
          "type": "array",
          "prefixItems": [ { "const": "Plain" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "description": "Single integer tag",
          "type": "array",
          "prefixItems": [ { "const": "With_int" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "description": "String and int",
          "type": "array",
          "prefixItems": [
            { "const": "Pair" }, { "type": "string" }, { "type": "integer" }
          ],
          "unevaluatedItems": false,
          "minItems": 3,
          "maxItems": 3
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "Description_value" },
            { "description": "A string value", "type": "string" }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
    |}]

type described_variant_inline_record =
  | Point of {
      x : int;
      y : int;
    } [@jsonschema.description "A 2D point"]
[@@deriving jsonschema]

let%expect_test "variant_inline_record_constructor_description" =
  print_schema described_variant_inline_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "description": "A 2D point",
          "type": "array",
          "prefixItems": [
            { "const": "Point" },
            {
              "type": "object",
              "properties": {
                "y": { "type": "integer" },
                "x": { "type": "integer" }
              },
              "required": [ "y", "x" ],
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

type described_variant_string =
  | A [@jsonschema.description "First choice"]
  | B [@jsonschema.description "Second choice"]
[@@deriving jsonschema ~variant_as_string]

let%expect_test "variant_constructor_description_variant_as_string" =
  print_schema described_variant_string_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        { "description": "First choice", "const": "A" },
        { "description": "Second choice", "const": "B" }
      ]
    }
    |}]

(* The [~ocaml_doc] flag opts into using [(** ... *)] doc comments as a
   fallback for [@jsonschema.description]. Without the flag, doc comments are
   ignored (see [ocaml_doc_disabled_by_default] below). *)

(** A user object *)
type doc_comment_record = {
  name : string;  (** The user's full name *)
  age : int;  (** The user's age *)
}
[@@deriving jsonschema ~ocaml_doc]

let%expect_test "ocaml_doc_fallback_for_record" =
  print_schema doc_comment_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "description": "A user object",
      "type": "object",
      "properties": {
        "age": { "description": "The user's age", "type": "integer" },
        "name": { "description": "The user's full name", "type": "string" }
      },
      "required": [ "age", "name" ],
      "additionalProperties": false
    }
    |}]

(* Without the [~ocaml_doc] flag, doc comments are not turned into descriptions. *)

(** A user object *)
type doc_comment_disabled = { name : string  (** The user's full name *) } [@@deriving jsonschema]

let%expect_test "ocaml_doc_disabled_by_default" =
  print_schema doc_comment_disabled_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": { "name": { "type": "string" } },
      "required": [ "name" ],
      "additionalProperties": false
    }
    |}]

(* Explicit [@jsonschema.description] wins over an ocaml.doc comment on the same node. *)
type doc_comment_override = { field : string [@jsonschema.description "explicit wins"]  (** ocaml.doc loses *) }
[@@deriving jsonschema ~ocaml_doc]

let%expect_test "ocaml_doc_overridden_by_jsonschema_description" =
  print_schema doc_comment_override_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "field": { "description": "explicit wins", "type": "string" }
      },
      "required": [ "field" ],
      "additionalProperties": false
    }
    |}]

type doc_comment_variant =
  | Plain  (** No payload *)
  | With_int of int  (** Single integer tag *)
[@@deriving jsonschema ~ocaml_doc]

let%expect_test "ocaml_doc_fallback_for_variant" =
  print_schema doc_comment_variant_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "description": "No payload",
          "type": "array",
          "prefixItems": [ { "const": "Plain" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "description": "Single integer tag",
          "type": "array",
          "prefixItems": [ { "const": "With_int" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
    |}]

type doc_comment_core_type = (string[@ocaml.doc " A string alias "]) [@@deriving jsonschema ~ocaml_doc]

let%expect_test "ocaml_doc_fallback_for_core_type" =
  print_schema doc_comment_core_type_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "description": "A string alias",
      "type": "string"
    }
    |}]

type doc_attribute_alias = (string[@doc " Alias fallback "]) [@@deriving jsonschema ~ocaml_doc]

let%expect_test "doc_attribute_alias_fallback" =
  print_schema doc_attribute_alias_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "description": "Alias fallback",
      "type": "string"
    }
    |}]

(* Multi-line doc comments are preserved as-is apart from a [String.trim] on the
   outermost whitespace. Internal newlines and indentation remain in the
   generated description. *)
type doc_comment_multiline = {
  name : string;  (** The user's full name.
          Must be non-empty and under 100 characters. *)
}
[@@deriving jsonschema ~ocaml_doc]

let%expect_test "ocaml_doc_multiline_preserves_internal_whitespace" =
  print_schema doc_comment_multiline_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "name": {
          "description": "The user's full name.\n          Must be non-empty and under 100 characters.",
          "type": "string"
        }
      },
      "required": [ "name" ],
      "additionalProperties": false
    }
    |}]

type doc_comment_poly_variant =
  [ `Plain  (** No payload *)
  | `With_int of int  (** Single integer tag *)
  ]
[@@deriving jsonschema ~ocaml_doc]

let%expect_test "ocaml_doc_fallback_for_polymorphic_variant" =
  print_schema doc_comment_poly_variant_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "description": "No payload",
          "type": "array",
          "prefixItems": [ { "const": "Plain" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "description": "Single integer tag",
          "type": "array",
          "prefixItems": [ { "const": "With_int" }, { "type": "integer" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
    |}]

(* Multiple hand-written [@ocaml.doc] / [@doc] attributes on the same node are
   joined into a single description with a blank-line separator. *)
type doc_comment_multiple = (string[@ocaml.doc " first block "] [@ocaml.doc " second block "] [@doc " third block "])
[@@deriving jsonschema ~ocaml_doc]

let%expect_test "ocaml_doc_multiple_attrs_joined" =
  print_schema doc_comment_multiple_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "description": "first block\n\nsecond block\n\nthird block",
      "type": "string"
    }
    |}]

(* Explicit [@jsonschema.description] on a polymorphic variant tag takes precedence. *)
type doc_comment_poly_variant_override = [ `Tagged [@jsonschema.description "explicit wins"]  (** ocaml.doc loses *) ]
[@@deriving jsonschema ~ocaml_doc]

let%expect_test "ocaml_doc_overridden_on_polymorphic_variant" =
  print_schema doc_comment_poly_variant_override_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "description": "explicit wins",
          "type": "array",
          "prefixItems": [ { "const": "Tagged" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        }
      ]
    }
    |}]

(* Top-level @@jsonschema.description on a variant (whole anyOf schema). *)
type computation_result =
  | Ok
  | Err of string
[@@deriving jsonschema] [@@jsonschema.description "Either success or an error message string"]

let%expect_test "variant_type_level_description" =
  print_schema computation_result_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "description": "Either success or an error message string",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [ { "const": "Ok" } ],
          "unevaluatedItems": false,
          "minItems": 1,
          "maxItems": 1
        },
        {
          "type": "array",
          "prefixItems": [ { "const": "Err" }, { "type": "string" } ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
    |}]

type nullable_fields = {
  plain : string option;
  drop_simple : string option; [@jsonschema.option]
  drop_complex : int list option; [@jsonschema.option]
}
[@@deriving jsonschema]

let%expect_test "nullable_fields" =
  print_schema nullable_fields_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "drop_complex": {
          "anyOf": [
            { "type": "array", "items": { "type": "integer" } },
            { "type": "null" }
          ]
        },
        "drop_simple": { "type": [ "string", "null" ] },
        "plain": { "type": [ "string", "null" ] }
      },
      "required": [ "plain" ],
      "additionalProperties": false
    }
    |}]

type composing_type = string
let composing_type_jsonschema = `Assoc [ "type", `String "string"; "description", `String "A string" ]

type composing_record = { composing_type : composing_type option [@jsonschema.option] } [@@deriving jsonschema]

let%expect_test "nullable_option_composing" =
  print_schema composing_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "composing_type": {
          "anyOf": [
            { "type": "string", "description": "A string" }, { "type": "null" }
          ]
        }
      },
      "required": [],
      "additionalProperties": false
    }
    |}]

type with_format = string [@@jsonschema.format "date-time"] [@@deriving jsonschema]

let%expect_test "with_format" =
  print_schema with_format_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "format": "date-time",
      "type": "string"
    }
    |}]

type with_format_record = { with_format : string [@jsonschema.format "date-time"] } [@@deriving jsonschema]

let%expect_test "with_format_record" =
  print_schema with_format_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "with_format": { "format": "date-time", "type": "string" }
      },
      "required": [ "with_format" ],
      "additionalProperties": false
    }
    |}]

type with_format_variant =
  | A of (string[@jsonschema.format "date-time"] [@jsonschema.description "A date-time string"])
  | B
[@@deriving jsonschema]

let%expect_test "with_format_variant" =
  print_schema with_format_variant_jsonschema;
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
              "format": "date-time",
              "description": "A date-time string",
              "type": "string"
            }
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
    |}]

(* Parametric recursive type: the recursive call carries type arguments *)
type 'a grade' =
  | A of 'a
  | B of ('a grade' * 'a grade')
  | C

type 'a grade = 'a grade' =
  | A of 'a
  | B of ('a grade * 'a grade)
  | C
[@@deriving jsonschema]

let%expect_test "grade" =
  print_schema (grade_jsonschema int_jsonschema);
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "grade": {
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
              "prefixItems": [
                { "const": "B" },
                {
                  "type": "array",
                  "prefixItems": [
                    { "$ref": "#/$defs/grade" }, { "$ref": "#/$defs/grade" }
                  ],
                  "unevaluatedItems": false,
                  "minItems": 2,
                  "maxItems": 2
                }
              ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "C" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            }
          ]
        }
      },
      "$ref": "#/$defs/grade"
    }
    |}]

(* Recursive type referenced in two fields of the same parent record *)
type self_ref = { children : self_ref list } [@@deriving jsonschema]

type two_self_refs = {
  a : self_ref;
  b : self_ref;
}
[@@deriving jsonschema]

let%expect_test "no_duplicate_id_when_recursive_type_used_twice" =
  print_schema two_self_refs_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "b": {
          "$id": "file://test/test.ml:2949",
          "$defs": {
            "self_ref": {
              "type": "object",
              "properties": {
                "children": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/self_ref" }
                }
              },
              "required": [ "children" ],
              "additionalProperties": false
            }
          },
          "$ref": "#/$defs/self_ref"
        },
        "a": {
          "$id": "file://test/test.ml:2948",
          "$defs": {
            "self_ref": {
              "type": "object",
              "properties": {
                "children": {
                  "type": "array",
                  "items": { "$ref": "#/$defs/self_ref" }
                }
              },
              "required": [ "children" ],
              "additionalProperties": false
            }
          },
          "$ref": "#/$defs/self_ref"
        }
      },
      "required": [ "b", "a" ],
      "additionalProperties": false
    }
    |}]

(* Polymorphic recursive type: the recursive reference carries type arguments *)
type ('atom, 'group_atom) filter =
  | Atom of 'atom
  | Group of ('atom, 'group_atom) filter list * 'group_atom
[@@deriving jsonschema]

type ('atom, 'group_atom) bool_filter =
  | BoolAtom of ('atom, 'group_atom) filter
  | BoolFilterGroup of ('atom, 'group_atom) bool_filter list
[@@deriving jsonschema]

let%expect_test "polymorphic_recursive_ref" =
  print_schema (filter_jsonschema int_jsonschema string_jsonschema);
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "filter": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "Atom" }, { "type": "integer" } ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            },
            {
              "type": "array",
              "prefixItems": [
                { "const": "Group" },
                { "type": "array", "items": { "$ref": "#/$defs/filter" } },
                { "type": "string" }
              ],
              "unevaluatedItems": false,
              "minItems": 3,
              "maxItems": 3
            }
          ]
        }
      },
      "$ref": "#/$defs/filter"
    }
    |}]

(* Cross-$ref bug: outer recursive type uses a parametric recursive type with
   itself as the type argument *)
type 'a rec_wrapper =
  | RWrap of 'a
  | RNested of 'a rec_wrapper
[@@deriving jsonschema]

type outer_rec =
  | ORLeaf of int
  | ORNode of outer_rec rec_wrapper
[@@deriving jsonschema]

let%expect_test "parametric_recursive_cross_ref" =
  print_schema outer_rec_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "outer_rec": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "ORLeaf" }, { "type": "integer" } ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            },
            {
              "type": "array",
              "prefixItems": [
                { "const": "ORNode" }, { "$ref": "#/$defs/rec_wrapper" }
              ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            }
          ]
        },
        "rec_wrapper": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [
                { "const": "RWrap" }, { "$ref": "#/$defs/outer_rec" }
              ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            },
            {
              "type": "array",
              "prefixItems": [
                { "const": "RNested" }, { "$ref": "#/$defs/rec_wrapper" }
              ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            }
          ]
        }
      },
      "$ref": "#/$defs/outer_rec"
    }
    |}]

let%expect_test "polymorphic_recursive_ref_bool_filter" =
  print_schema (bool_filter_jsonschema int_jsonschema string_jsonschema);
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$defs": {
        "bool_filter": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [
                { "const": "BoolAtom" },
                {
                  "$id": "file://test/test.ml:3008",
                  "$defs": {
                    "filter": {
                      "anyOf": [
                        {
                          "type": "array",
                          "prefixItems": [
                            { "const": "Atom" }, { "type": "integer" }
                          ],
                          "unevaluatedItems": false,
                          "minItems": 2,
                          "maxItems": 2
                        },
                        {
                          "type": "array",
                          "prefixItems": [
                            { "const": "Group" },
                            {
                              "type": "array",
                              "items": { "$ref": "#/$defs/filter" }
                            },
                            { "type": "string" }
                          ],
                          "unevaluatedItems": false,
                          "minItems": 3,
                          "maxItems": 3
                        }
                      ]
                    }
                  },
                  "$ref": "#/$defs/filter"
                }
              ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            },
            {
              "type": "array",
              "prefixItems": [
                { "const": "BoolFilterGroup" },
                { "type": "array", "items": { "$ref": "#/$defs/bool_filter" } }
              ],
              "unevaluatedItems": false,
              "minItems": 2,
              "maxItems": 2
            }
          ]
        }
      },
      "$ref": "#/$defs/bool_filter"
    }
    |}]

type with_maximum = int [@@jsonschema.maximum 100] [@@deriving jsonschema]

let%expect_test "with_maximum" =
  print_schema with_maximum_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "maximum": 100,
      "type": "integer"
    }
    |}]

type with_maximum_record = { field : int [@jsonschema.maximum 100] } [@@deriving jsonschema]

let%expect_test "with_maximum" =
  print_schema with_maximum_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": { "field": { "maximum": 100, "type": "integer" } },
      "required": [ "field" ],
      "additionalProperties": false
    }
    |}]

(* [@jsonschema.attrs] — composite attribute bundling multiple annotations *)

type attrs_core_type =
  (int[@jsonschema.attrs { maximum = 100; minimum = 0; description = "Integer percentage value (0-100 inclusive)" }])
[@@deriving jsonschema]

let%expect_test "attrs_core_type" =
  print_schema attrs_core_type_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "description": "Integer percentage value (0-100 inclusive)",
      "minimum": 0,
      "maximum": 100,
      "type": "integer"
    }
    |}]

type attrs_record = {
  score : int; [@jsonschema.attrs { maximum = 100; minimum = 0; description = "Score out of 100" }]
  label : string; [@jsonschema.attrs { format = "date-time"; description = "An ISO date-time" }]
}
[@@deriving jsonschema]

let%expect_test "attrs_record" =
  print_schema attrs_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "label": {
          "description": "An ISO date-time",
          "format": "date-time",
          "type": "string"
        },
        "score": {
          "description": "Score out of 100",
          "minimum": 0,
          "maximum": 100,
          "type": "integer"
        }
      },
      "required": [ "label", "score" ],
      "additionalProperties": false
    }
    |}]

type attrs_type_decl = int [@@jsonschema.attrs { description = "A plain integer" }] [@@deriving jsonschema]

let%expect_test "attrs_type_decl" =
  print_schema attrs_type_decl_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "description": "A plain integer",
      "type": "integer"
    }
    |}]

(* [@jsonschema.minimum] and [@jsonschema.maximum] — all annotation contexts *)

(* core type: int *)
type minimum_core_type_int = (int[@jsonschema.minimum 0] [@jsonschema.maximum 100]) [@@deriving jsonschema]

let%expect_test "minimum_maximum_core_type_int" =
  print_schema minimum_core_type_int_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "minimum": 0,
      "maximum": 100,
      "type": "integer"
    }
    |}]

(* core type: float *)
type minimum_core_type_float = (float[@jsonschema.minimum 0.0] [@jsonschema.maximum 1.0]) [@@deriving jsonschema]

let%expect_test "minimum_maximum_core_type_float" =
  print_schema minimum_core_type_float_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "minimum": 0.0,
      "maximum": 1.0,
      "type": "number"
    }
    |}]

(* label declaration: int and float fields *)
type minimum_maximum_record = {
  score : int; [@jsonschema.minimum 0] [@jsonschema.maximum 100]
  ratio : float; [@jsonschema.minimum 0.0] [@jsonschema.maximum 1.0]
}
[@@deriving jsonschema]

let%expect_test "minimum_maximum_record" =
  print_schema minimum_maximum_record_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "ratio": { "minimum": 0.0, "maximum": 1.0, "type": "number" },
        "score": { "minimum": 0, "maximum": 100, "type": "integer" }
      },
      "required": [ "ratio", "score" ],
      "additionalProperties": false
    }
    |}]

(* type declaration: int *)
type minimum_maximum_type_decl_int = int [@@jsonschema.minimum 0] [@@jsonschema.maximum 255] [@@deriving jsonschema]

let%expect_test "minimum_maximum_type_decl_int" =
  print_schema minimum_maximum_type_decl_int_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "minimum": 0,
      "maximum": 255,
      "type": "integer"
    }
    |}]

(* type declaration: float *)
type minimum_maximum_type_decl_float = float
[@@jsonschema.minimum 0.0] [@@jsonschema.maximum 1.0] [@@deriving jsonschema]

let%expect_test "minimum_maximum_type_decl_float" =
  print_schema minimum_maximum_type_decl_float_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "minimum": 0.0,
      "maximum": 1.0,
      "type": "number"
    }
    |}]

(* variant payload: core type annotation *)
type minimum_maximum_variant =
  | Percentage of (int[@jsonschema.minimum 0] [@jsonschema.maximum 100])
  | Factor of (float[@jsonschema.minimum 0.0] [@jsonschema.maximum 1.0])
[@@deriving jsonschema]

let%expect_test "minimum_maximum_variant" =
  print_schema minimum_maximum_variant_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        {
          "type": "array",
          "prefixItems": [
            { "const": "Percentage" },
            { "minimum": 0, "maximum": 100, "type": "integer" }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        },
        {
          "type": "array",
          "prefixItems": [
            { "const": "Factor" },
            { "minimum": 0.0, "maximum": 1.0, "type": "number" }
          ],
          "unevaluatedItems": false,
          "minItems": 2,
          "maxItems": 2
        }
      ]
    }
    |}]

type variant_for_default =
  | A
  | B
[@@deriving jsonschema]

let variant_for_default_to_json = function
  | A -> `List [ `String "A" ]
  | B -> `List [ `String "B" ]

type record_for_default = { score : int option } [@@deriving jsonschema]

let record_for_default_to_json { score } =
  `Assoc
    [
      ( "score",
        match score with
        | None -> `Null
        | Some x -> `Int x );
    ]

type default_value = {
  score : int option; [@default 0]
  label : string; [@jsonschema.default "default"]
  speed : float; [@jsonschema.default 100.0]
  is_active : bool; [@jsonschema.default false]
  variant : variant_for_default; [@jsonschema.default A]
  record : record_for_default; [@jsonschema.default { score = None }]
  int_list : int list; [@jsonschema.default [ 1; 2; 3 ]]
}
[@@deriving jsonschema]

let%expect_test "default_value" =
  print_schema default_value_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "int_list": {
          "default": [ 1, 2, 3 ],
          "type": "array",
          "items": { "type": "integer" }
        },
        "record": {
          "default": { "score": null },
          "type": "object",
          "properties": { "score": { "type": [ "integer", "null" ] } },
          "required": [ "score" ],
          "additionalProperties": false
        },
        "variant": {
          "default": [ "A" ],
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
        },
        "is_active": { "default": false, "type": "boolean" },
        "speed": { "default": 100.0, "type": "number" },
        "label": { "default": "default", "type": "string" },
        "score": { "default": 0, "type": [ "integer", "null" ] }
      },
      "required": [],
      "additionalProperties": false
    }
    |}]

module Status = struct
  type t =
    | Active
    | Inactive
  [@@deriving jsonschema]

  let to_json = function
    | Active -> `List [ `String "Active" ]
    | Inactive -> `List [ `String "Inactive" ]
end

type default_with_module_type = { status : Status.t [@jsonschema.default Status.Active] } [@@deriving jsonschema]

let%expect_test "default_with_module_type" =
  print_schema default_with_module_type_jsonschema;
  [%expect
    {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "status": {
          "default": [ "Active" ],
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [ { "const": "Active" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            },
            {
              "type": "array",
              "prefixItems": [ { "const": "Inactive" } ],
              "unevaluatedItems": false,
              "minItems": 1,
              "maxItems": 1
            }
          ]
        }
      },
      "required": [],
      "additionalProperties": false
    }
    |}]
