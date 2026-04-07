[@@@ocaml.warning "-37-69"]
let print_schema ?definitions  ?id  ?title  ?description  s =
  let s =
    Ppx_deriving_jsonschema_runtime.json_schema ?definitions ?id ?title
      ?description s in
  let () = print_endline (Yojson.Basic.pretty_to_string s) in ()
let string_jsonschema = `Assoc [("type", (`String "string"))]
let int_jsonschema = `Assoc [("type", (`String "integer"))]
let bool_jsonschema = `Assoc [("type", (`String "boolean"))]
module Mod1 =
  struct
    type m_1 =
      | A 
      | B [@@deriving jsonschema]
    include
      struct
        let m_1_jsonschema =
          `Assoc
            [("anyOf",
               (`List
                  [`Assoc
                     [("type", (`String "array"));
                     ("prefixItems",
                       (`List [`Assoc [("const", (`String "A"))]]));
                     ("unevaluatedItems", (`Bool false));
                     ("minItems", (`Int 1));
                     ("maxItems", (`Int 1))];
                  `Assoc
                    [("type", (`String "array"));
                    ("prefixItems",
                      (`List [`Assoc [("const", (`String "B"))]]));
                    ("unevaluatedItems", (`Bool false));
                    ("minItems", (`Int 1));
                    ("maxItems", (`Int 1))]]))][@@warning "-32-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    [%%expect_test
      let "m_1" =
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
      |}]]
    module Mod2 =
      struct
        type m_2 =
          | C 
          | D [@@deriving jsonschema]
        include
          struct
            let m_2_jsonschema =
              `Assoc
                [("anyOf",
                   (`List
                      [`Assoc
                         [("type", (`String "array"));
                         ("prefixItems",
                           (`List [`Assoc [("const", (`String "C"))]]));
                         ("unevaluatedItems", (`Bool false));
                         ("minItems", (`Int 1));
                         ("maxItems", (`Int 1))];
                      `Assoc
                        [("type", (`String "array"));
                        ("prefixItems",
                          (`List [`Assoc [("const", (`String "D"))]]));
                        ("unevaluatedItems", (`Bool false));
                        ("minItems", (`Int 1));
                        ("maxItems", (`Int 1))]]))][@@warning "-32-39"]
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
        [%%expect_test
          let "m_2" =
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
        |}]]
      end
  end
type with_modules = {
  m: Mod1.m_1 ;
  m2: Mod1.Mod2.m_2 }[@@deriving jsonschema]
include
  struct
    let with_modules_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("m2",
                ((match Mod1.Mod2.m_2_jsonschema with
                  | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                      `Assoc
                        (("$id", (`String "urn:jsonschema:test.ml:78:1882"))
                        :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                  | other -> other)));
             ("m",
               ((match Mod1.m_1_jsonschema with
                 | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                     `Assoc
                       (("$id", (`String "urn:jsonschema:test.ml:77:1865"))
                       :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                 | other -> other)))]));
        ("required", (`List [`String "m2"; `String "m"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "with_modules" =
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
    |}]]
type kind =
  | Success 
  | Error 
  | Skipped [@name "skipped"][@@deriving jsonschema]
include
  struct
    let kind_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List [`Assoc [("const", (`String "Success"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 1));
                 ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List [`Assoc [("const", (`String "Error"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 1));
                ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List [`Assoc [("const", (`String "skipped"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 1));
                ("maxItems", (`Int 1))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "kind" =
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
    |}]]
type kind_as_string =
  | Success 
  | Error 
  | Skipped [@name "skipped"][@@deriving jsonschema ~variant_as_string]
include
  struct
    let kind_as_string_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "Success"))];
              `Assoc [("const", (`String "Error"))];
              `Assoc [("const", (`String "skipped"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "kind_as_string" =
    print_schema kind_as_string_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        { "const": "Success" }, { "const": "Error" }, { "const": "skipped" }
      ]
    }
    |}]]
type poly_kind = [ `Aaa  | `Bbb  | `Ccc [@name "ccc"]][@@deriving jsonschema]
include
  struct
    let poly_kind_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List [`Assoc [("const", (`String "Aaa"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 1));
                 ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List [`Assoc [("const", (`String "Bbb"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 1));
                ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List [`Assoc [("const", (`String "ccc"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 1));
                ("maxItems", (`Int 1))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_kind" =
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
    |}]]
type poly_kind_as_string = [ `Aaa  | `Bbb  | `Ccc [@name "ccc"]][@@deriving
                                                                  jsonschema
                                                                    ~variant_as_string]
include
  struct
    let poly_kind_as_string_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "Aaa"))];
              `Assoc [("const", (`String "Bbb"))];
              `Assoc [("const", (`String "ccc"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_kind_as_string" =
    print_schema poly_kind_as_string_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Aaa" }, { "const": "Bbb" }, { "const": "ccc" } ]
    }
    |}]]
type poly_kind_with_payload =
  [ `Aaa of int  | `Bbb  | `Ccc of (string * bool) [@name "ccc"]][@@deriving
                                                                   jsonschema]
include
  struct
    let poly_kind_with_payload_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "Aaa"))];
                      `Assoc [("type", (`String "integer"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 2));
                 ("maxItems", (`Int 2))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List [`Assoc [("const", (`String "Bbb"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 1));
                ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List
                     [`Assoc [("const", (`String "ccc"))];
                     `Assoc [("type", (`String "string"))];
                     `Assoc [("type", (`String "boolean"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 3));
                ("maxItems", (`Int 3))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_kind_with_payload" =
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
    |}]]
type poly_kind_with_payload_as_string =
  [ `Aaa of int  | `Bbb  | `Ccc of (string * bool) [@name "ccc"]][@@deriving
                                                                   jsonschema
                                                                    ~variant_as_string]
include
  struct
    let poly_kind_with_payload_as_string_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "Aaa"))];
              `Assoc [("const", (`String "Bbb"))];
              `Assoc [("const", (`String "ccc"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_kind_with_payload_as_string" =
    print_schema poly_kind_with_payload_as_string_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Aaa" }, { "const": "Bbb" }, { "const": "ccc" } ]
    }
    |}]]
type poly_inherit = [ `New_one  | `Second_one of int  | poly_kind][@@deriving
                                                                    jsonschema]
include
  struct
    let poly_inherit_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List [`Assoc [("const", (`String "New_one"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 1));
                 ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List
                     [`Assoc [("const", (`String "Second_one"))];
                     `Assoc [("type", (`String "integer"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 2));
                ("maxItems", (`Int 2))];
              (match poly_kind_jsonschema with
               | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                   `Assoc
                     (("$id", (`String "urn:jsonschema:test.ml:305:7119")) ::
                     (List.filter (fun (k, _) -> k <> "$id") pairs))
               | other -> other)]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_inherit" =
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
    |}]]
type poly_inherit_as_string =
  [ `New_one  | `Second_one of int  | poly_kind_as_string][@@deriving
                                                            jsonschema
                                                              ~variant_as_string]
include
  struct
    let poly_inherit_as_string_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "New_one"))];
              `Assoc [("const", (`String "Second_one"))];
              (match poly_kind_as_string_jsonschema with
               | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                   `Assoc
                     (("$id", (`String "urn:jsonschema:test.ml:362:8515")) ::
                     (List.filter (fun (k, _) -> k <> "$id") pairs))
               | other -> other)]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_inherit_as_string" =
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
    |}]]
type event =
  {
  date: float ;
  kind_f: kind ;
  comment: string ;
  opt: int option [@key "opt_int"];
  a: float array ;
  l: string list ;
  t: [ `Foo  | `Bar  | `Baz ] ;
  c: char ;
  bunch_of_bytes: bytes ;
  string_ref: string ref ;
  unit: unit ;
  native_int: nativeint }[@@deriving jsonschema]
include
  struct
    let event_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("native_int", (`Assoc [("type", (`String "integer"))]));
             ("unit", (`Assoc [("type", (`String "null"))]));
             ("string_ref", (`Assoc [("type", (`String "string"))]));
             ("bunch_of_bytes", (`Assoc [("type", (`String "string"))]));
             ("c",
               (`Assoc
                  [("type", (`String "string"));
                  ("minLength", (`Int 1));
                  ("maxLength", (`Int 1))]));
             ("t",
               (`Assoc
                  [("anyOf",
                     (`List
                        [`Assoc
                           [("type", (`String "array"));
                           ("prefixItems",
                             (`List [`Assoc [("const", (`String "Foo"))]]));
                           ("unevaluatedItems", (`Bool false));
                           ("minItems", (`Int 1));
                           ("maxItems", (`Int 1))];
                        `Assoc
                          [("type", (`String "array"));
                          ("prefixItems",
                            (`List [`Assoc [("const", (`String "Bar"))]]));
                          ("unevaluatedItems", (`Bool false));
                          ("minItems", (`Int 1));
                          ("maxItems", (`Int 1))];
                        `Assoc
                          [("type", (`String "array"));
                          ("prefixItems",
                            (`List [`Assoc [("const", (`String "Baz"))]]));
                          ("unevaluatedItems", (`Bool false));
                          ("minItems", (`Int 1));
                          ("maxItems", (`Int 1))]]))]));
             ("l",
               (`Assoc
                  [("type", (`String "array"));
                  ("items", (`Assoc [("type", (`String "string"))]))]));
             ("a",
               (`Assoc
                  [("type", (`String "array"));
                  ("items", (`Assoc [("type", (`String "number"))]))]));
             ("opt_int",
               ((match `Assoc [("type", (`String "integer"))] with
                 | `Assoc (("type", `String t)::[]) ->
                     `Assoc [("type", (`List [`String t; `String "null"]))]
                 | s ->
                     `Assoc
                       [("anyOf",
                          (`List [s; `Assoc [("type", (`String "null"))]]))])));
             ("comment", (`Assoc [("type", (`String "string"))]));
             ("kind_f",
               ((match kind_jsonschema with
                 | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                     `Assoc
                       (("$id", (`String "urn:jsonschema:test.ml:384:9016"))
                       :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                 | other -> other)));
             ("date", (`Assoc [("type", (`String "number"))]))]));
        ("required",
          (`List
             [`String "native_int";
             `String "unit";
             `String "string_ref";
             `String "bunch_of_bytes";
             `String "c";
             `String "t";
             `String "l";
             `String "a";
             `String "opt_int";
             `String "comment";
             `String "kind_f";
             `String "date"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "event" =
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
    |}]]
type recursive_record = {
  a: int ;
  b: recursive_record list }[@@deriving jsonschema]
include
  struct
    let recursive_record_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("b",
                  (`Assoc
                     [("type", (`String "array"));
                     ("items",
                       (`Assoc
                          [("$ref", (`String "#/$defs/recursive_record"))]))]));
               ("a", (`Assoc [("type", (`String "integer"))]))]));
          ("required", (`List [`String "b"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:recursive_record"));
        ("$defs", (`Assoc (("recursive_record", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/recursive_record"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "recursive_record" =
    print_schema recursive_record_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:recursive_record",
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
    |}]]
type recursive_variant =
  | A of recursive_variant 
  | B [@@deriving jsonschema]
include
  struct
    let recursive_variant_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "A"))];
                        `Assoc
                          [("$ref", (`String "#/$defs/recursive_variant"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List [`Assoc [("const", (`String "B"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 1));
                  ("maxItems", (`Int 1))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:recursive_variant"));
        ("$defs", (`Assoc (("recursive_variant", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/recursive_variant"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "recursive_variant" =
    print_schema recursive_variant_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:recursive_variant",
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
    |}]]
type tree =
  | Leaf 
  | Node of {
  value: int ;
  left: tree ;
  right: tree } [@@deriving jsonschema]
include
  struct
    let tree_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List [`Assoc [("const", (`String "Leaf"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 1));
                   ("maxItems", (`Int 1))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Node"))];
                       `Assoc
                         [("type", (`String "object"));
                         ("properties",
                           (`Assoc
                              [("right",
                                 (`Assoc [("$ref", (`String "#/$defs/tree"))]));
                              ("left",
                                (`Assoc [("$ref", (`String "#/$defs/tree"))]));
                              ("value",
                                (`Assoc [("type", (`String "integer"))]))]));
                         ("required",
                           (`List
                              [`String "right";
                              `String "left";
                              `String "value"]));
                         ("additionalProperties", (`Bool false))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:tree"));
        ("$defs", (`Assoc (("tree", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/tree"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "tree" =
    print_schema tree_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:tree",
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
    |}]]
type non_recursive = {
  x: int ;
  y: string }[@@deriving jsonschema]
include
  struct
    let non_recursive_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("y", (`Assoc [("type", (`String "string"))]));
             ("x", (`Assoc [("type", (`String "integer"))]))]));
        ("required", (`List [`String "y"; `String "x"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "non_recursive" =
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
    |}]]
type foo = {
  bar: bar option }
and bar = {
  foo: foo option }[@@deriving jsonschema]
include
  struct
    let foo_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body_0 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("bar",
                  ((match `Assoc [("$ref", (`String "#/$defs/bar"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "bar"]));
          ("additionalProperties", (`Bool false))] in
      let _ppx_body_1 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("foo",
                  ((match `Assoc [("$ref", (`String "#/$defs/foo"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "foo"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:foo"));
        ("$defs",
          (`Assoc
             ([("foo", _ppx_body_0); ("bar", _ppx_body_1)] @ (!_ppx_eds))));
        ("$ref", (`String "#/$defs/foo"))][@@warning "-32-39"]
    let bar_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body_0 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("bar",
                  ((match `Assoc [("$ref", (`String "#/$defs/bar"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "bar"]));
          ("additionalProperties", (`Bool false))] in
      let _ppx_body_1 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("foo",
                  ((match `Assoc [("$ref", (`String "#/$defs/foo"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "foo"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:bar"));
        ("$defs",
          (`Assoc
             ([("foo", _ppx_body_0); ("bar", _ppx_body_1)] @ (!_ppx_eds))));
        ("$ref", (`String "#/$defs/bar"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "mutually_recursive_foo" =
    print_schema foo_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:foo",
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
    |}]]
[%%expect_test
  let "mutually_recursive_bar" =
    print_schema bar_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:bar",
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
    |}]]
type expr =
  | Literal of int 
  | Binary of expr * expr 
  | Block of stmt list 
and stmt =
  | ExprStmt of expr 
  | IfStmt of {
  cond: expr ;
  then_: stmt ;
  else_: stmt option } [@@deriving jsonschema]
include
  struct
    let expr_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body_0 =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "Literal"))];
                        `Assoc [("type", (`String "integer"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Binary"))];
                       `Assoc [("$ref", (`String "#/$defs/expr"))];
                       `Assoc [("$ref", (`String "#/$defs/expr"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 3));
                  ("maxItems", (`Int 3))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Block"))];
                       `Assoc
                         [("type", (`String "array"));
                         ("items",
                           (`Assoc [("$ref", (`String "#/$defs/stmt"))]))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      let _ppx_body_1 =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "ExprStmt"))];
                        `Assoc [("$ref", (`String "#/$defs/expr"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "IfStmt"))];
                       `Assoc
                         [("type", (`String "object"));
                         ("properties",
                           (`Assoc
                              [("else_",
                                 ((match `Assoc
                                           [("$ref",
                                              (`String "#/$defs/stmt"))]
                                   with
                                   | `Assoc (("type", `String t)::[]) ->
                                       `Assoc
                                         [("type",
                                            (`List
                                               [`String t; `String "null"]))]
                                   | s ->
                                       `Assoc
                                         [("anyOf",
                                            (`List
                                               [s;
                                               `Assoc
                                                 [("type", (`String "null"))]]))])));
                              ("then_",
                                (`Assoc [("$ref", (`String "#/$defs/stmt"))]));
                              ("cond",
                                (`Assoc [("$ref", (`String "#/$defs/expr"))]))]));
                         ("required",
                           (`List
                              [`String "else_";
                              `String "then_";
                              `String "cond"]));
                         ("additionalProperties", (`Bool false))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:expr"));
        ("$defs",
          (`Assoc
             ([("expr", _ppx_body_0); ("stmt", _ppx_body_1)] @ (!_ppx_eds))));
        ("$ref", (`String "#/$defs/expr"))][@@warning "-32-39"]
    let stmt_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body_0 =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "Literal"))];
                        `Assoc [("type", (`String "integer"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Binary"))];
                       `Assoc [("$ref", (`String "#/$defs/expr"))];
                       `Assoc [("$ref", (`String "#/$defs/expr"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 3));
                  ("maxItems", (`Int 3))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Block"))];
                       `Assoc
                         [("type", (`String "array"));
                         ("items",
                           (`Assoc [("$ref", (`String "#/$defs/stmt"))]))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      let _ppx_body_1 =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "ExprStmt"))];
                        `Assoc [("$ref", (`String "#/$defs/expr"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "IfStmt"))];
                       `Assoc
                         [("type", (`String "object"));
                         ("properties",
                           (`Assoc
                              [("else_",
                                 ((match `Assoc
                                           [("$ref",
                                              (`String "#/$defs/stmt"))]
                                   with
                                   | `Assoc (("type", `String t)::[]) ->
                                       `Assoc
                                         [("type",
                                            (`List
                                               [`String t; `String "null"]))]
                                   | s ->
                                       `Assoc
                                         [("anyOf",
                                            (`List
                                               [s;
                                               `Assoc
                                                 [("type", (`String "null"))]]))])));
                              ("then_",
                                (`Assoc [("$ref", (`String "#/$defs/stmt"))]));
                              ("cond",
                                (`Assoc [("$ref", (`String "#/$defs/expr"))]))]));
                         ("required",
                           (`List
                              [`String "else_";
                              `String "then_";
                              `String "cond"]));
                         ("additionalProperties", (`Bool false))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:stmt"));
        ("$defs",
          (`Assoc
             ([("expr", _ppx_body_0); ("stmt", _ppx_body_1)] @ (!_ppx_eds))));
        ("$ref", (`String "#/$defs/stmt"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "mutually_recursive_expr" =
    print_schema expr_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:expr",
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
    |}]]
type alpha = {
  x: int }
and beta = {
  y: string }[@@deriving jsonschema]
include
  struct
    let alpha_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc [("x", (`Assoc [("type", (`String "integer"))]))]));
        ("required", (`List [`String "x"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
    let beta_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc [("y", (`Assoc [("type", (`String "string"))]))]));
        ("required", (`List [`String "y"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "non_recursive_mutual_alpha" =
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
    |}]]
[%%expect_test
  let "non_recursive_mutual_beta" =
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
    |}]]
type node_a = {
  b: node_b option ;
  c: node_c option }
and node_b = {
  a: node_a option ;
  c: node_c option }
and node_c = {
  a: node_a option ;
  b: node_b option }[@@deriving jsonschema]
include
  struct
    let node_a_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body_0 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  ((match `Assoc [("$ref", (`String "#/$defs/node_c"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])));
               ("b",
                 ((match `Assoc [("$ref", (`String "#/$defs/node_b"))] with
                   | `Assoc (("type", `String t)::[]) ->
                       `Assoc [("type", (`List [`String t; `String "null"]))]
                   | s ->
                       `Assoc
                         [("anyOf",
                            (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "c"; `String "b"]));
          ("additionalProperties", (`Bool false))] in
      let _ppx_body_1 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  ((match `Assoc [("$ref", (`String "#/$defs/node_c"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])));
               ("a",
                 ((match `Assoc [("$ref", (`String "#/$defs/node_a"))] with
                   | `Assoc (("type", `String t)::[]) ->
                       `Assoc [("type", (`List [`String t; `String "null"]))]
                   | s ->
                       `Assoc
                         [("anyOf",
                            (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "c"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      let _ppx_body_2 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("b",
                  ((match `Assoc [("$ref", (`String "#/$defs/node_b"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])));
               ("a",
                 ((match `Assoc [("$ref", (`String "#/$defs/node_a"))] with
                   | `Assoc (("type", `String t)::[]) ->
                       `Assoc [("type", (`List [`String t; `String "null"]))]
                   | s ->
                       `Assoc
                         [("anyOf",
                            (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "b"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:node_a"));
        ("$defs",
          (`Assoc
             ([("node_a", _ppx_body_0);
              ("node_b", _ppx_body_1);
              ("node_c", _ppx_body_2)] @ (!_ppx_eds))));
        ("$ref", (`String "#/$defs/node_a"))][@@warning "-32-39"]
    let node_b_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body_0 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  ((match `Assoc [("$ref", (`String "#/$defs/node_c"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])));
               ("b",
                 ((match `Assoc [("$ref", (`String "#/$defs/node_b"))] with
                   | `Assoc (("type", `String t)::[]) ->
                       `Assoc [("type", (`List [`String t; `String "null"]))]
                   | s ->
                       `Assoc
                         [("anyOf",
                            (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "c"; `String "b"]));
          ("additionalProperties", (`Bool false))] in
      let _ppx_body_1 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  ((match `Assoc [("$ref", (`String "#/$defs/node_c"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])));
               ("a",
                 ((match `Assoc [("$ref", (`String "#/$defs/node_a"))] with
                   | `Assoc (("type", `String t)::[]) ->
                       `Assoc [("type", (`List [`String t; `String "null"]))]
                   | s ->
                       `Assoc
                         [("anyOf",
                            (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "c"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      let _ppx_body_2 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("b",
                  ((match `Assoc [("$ref", (`String "#/$defs/node_b"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])));
               ("a",
                 ((match `Assoc [("$ref", (`String "#/$defs/node_a"))] with
                   | `Assoc (("type", `String t)::[]) ->
                       `Assoc [("type", (`List [`String t; `String "null"]))]
                   | s ->
                       `Assoc
                         [("anyOf",
                            (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "b"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:node_b"));
        ("$defs",
          (`Assoc
             ([("node_a", _ppx_body_0);
              ("node_b", _ppx_body_1);
              ("node_c", _ppx_body_2)] @ (!_ppx_eds))));
        ("$ref", (`String "#/$defs/node_b"))][@@warning "-32-39"]
    let node_c_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body_0 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  ((match `Assoc [("$ref", (`String "#/$defs/node_c"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])));
               ("b",
                 ((match `Assoc [("$ref", (`String "#/$defs/node_b"))] with
                   | `Assoc (("type", `String t)::[]) ->
                       `Assoc [("type", (`List [`String t; `String "null"]))]
                   | s ->
                       `Assoc
                         [("anyOf",
                            (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "c"; `String "b"]));
          ("additionalProperties", (`Bool false))] in
      let _ppx_body_1 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  ((match `Assoc [("$ref", (`String "#/$defs/node_c"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])));
               ("a",
                 ((match `Assoc [("$ref", (`String "#/$defs/node_a"))] with
                   | `Assoc (("type", `String t)::[]) ->
                       `Assoc [("type", (`List [`String t; `String "null"]))]
                   | s ->
                       `Assoc
                         [("anyOf",
                            (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "c"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      let _ppx_body_2 =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("b",
                  ((match `Assoc [("$ref", (`String "#/$defs/node_b"))] with
                    | `Assoc (("type", `String t)::[]) ->
                        `Assoc
                          [("type", (`List [`String t; `String "null"]))]
                    | s ->
                        `Assoc
                          [("anyOf",
                             (`List [s; `Assoc [("type", (`String "null"))]]))])));
               ("a",
                 ((match `Assoc [("$ref", (`String "#/$defs/node_a"))] with
                   | `Assoc (("type", `String t)::[]) ->
                       `Assoc [("type", (`List [`String t; `String "null"]))]
                   | s ->
                       `Assoc
                         [("anyOf",
                            (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
          ("required", (`List [`String "b"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:node_c"));
        ("$defs",
          (`Assoc
             ([("node_a", _ppx_body_0);
              ("node_b", _ppx_body_1);
              ("node_c", _ppx_body_2)] @ (!_ppx_eds))));
        ("$ref", (`String "#/$defs/node_c"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "three_way_mutual_recursion" =
    print_schema node_a_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:node_a",
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
    |}]]
type recursive_tuple =
  | Leaf of int 
  | Branch of (recursive_tuple * recursive_tuple) [@@deriving jsonschema]
include
  struct
    let recursive_tuple_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "Leaf"))];
                        `Assoc [("type", (`String "integer"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Branch"))];
                       `Assoc
                         [("type", (`String "array"));
                         ("prefixItems",
                           (`List
                              [`Assoc
                                 [("$ref",
                                    (`String "#/$defs/recursive_tuple"))];
                              `Assoc
                                [("$ref",
                                   (`String "#/$defs/recursive_tuple"))]]));
                         ("unevaluatedItems", (`Bool false));
                         ("minItems", (`Int 2));
                         ("maxItems", (`Int 2))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:recursive_tuple"));
        ("$defs", (`Assoc (("recursive_tuple", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/recursive_tuple"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "recursive_tuple" =
    print_schema recursive_tuple_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:recursive_tuple",
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
    |}]]
type int_tree = tree[@@deriving jsonschema]
include
  struct
    let int_tree_jsonschema =
      match tree_jsonschema with
      | `Assoc pairs when List.mem_assoc "$defs" pairs ->
          `Assoc (("$id", (`String "urn:jsonschema:test.ml:920:23215")) ::
            (List.filter (fun (k, _) -> k <> "$id") pairs))
      | other -> other[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "recursive_abstract_alias" =
    print_schema int_tree_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:test/test.ml:920:23215",
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
    |}]]
type events = event list[@@deriving jsonschema]
include
  struct
    let events_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          ((match event_jsonschema with
            | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                `Assoc (("$id", (`String "urn:jsonschema:test.ml:965:24467"))
                  :: (List.filter (fun (k, _) -> k <> "$id") pairs))
            | other -> other)))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "events" =
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
    |}]]
type eventss = event list list[@@deriving jsonschema]
include
  struct
    let eventss_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          (`Assoc
             [("type", (`String "array"));
             ("items",
               ((match event_jsonschema with
                 | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                     `Assoc
                       (("$id",
                          (`String "urn:jsonschema:test.ml:1047:27015"))
                       :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                 | other -> other)))]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "eventss" =
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
    |}]]
type event_comment = (event * string)[@@deriving jsonschema]
include
  struct
    let event_comment_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("prefixItems",
          (`List
             [(match event_jsonschema with
               | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                   `Assoc
                     (("$id", (`String "urn:jsonschema:test.ml:1132:29766"))
                     :: (List.filter (fun (k, _) -> k <> "$id") pairs))
               | other -> other);
             `Assoc [("type", (`String "string"))]]));
        ("unevaluatedItems", (`Bool false));
        ("minItems", (`Int 2));
        ("maxItems", (`Int 2))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "event_comment" =
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
    |}]]
type event_comments' = event_comment list[@@deriving jsonschema]
include
  struct
    let event_comments'_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          ((match event_comment_jsonschema with
            | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                `Assoc
                  (("$id", (`String "urn:jsonschema:test.ml:1220:32607")) ::
                  (List.filter (fun (k, _) -> k <> "$id") pairs))
            | other -> other)))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "event_comments'" =
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
    |}]]
type event_n = (event * int) list[@@deriving jsonschema]
include
  struct
    let event_n_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          (`Assoc
             [("type", (`String "array"));
             ("prefixItems",
               (`List
                  [(match event_jsonschema with
                    | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                        `Assoc
                          (("$id",
                             (`String "urn:jsonschema:test.ml:1311:35651"))
                          :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                    | other -> other);
                  `Assoc [("type", (`String "integer"))]]));
             ("unevaluatedItems", (`Bool false));
             ("minItems", (`Int 2));
             ("maxItems", (`Int 2))]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "event_n" =
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
    |}]]
type events_array = events array[@@deriving jsonschema]
include
  struct
    let events_array_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          ((match events_jsonschema with
            | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                `Assoc
                  (("$id", (`String "urn:jsonschema:test.ml:1402:38683")) ::
                  (List.filter (fun (k, _) -> k <> "$id") pairs))
            | other -> other)))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "events_array" =
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
    |}]]
type numbers = int list[@@deriving jsonschema]
include
  struct
    let numbers_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items", (`Assoc [("type", (`String "integer"))]))][@@warning
                                                              "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "numbers" =
    print_schema numbers_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "items": { "type": "integer" }
    } |}]]
type opt = int option[@@deriving jsonschema]
include
  struct
    let opt_jsonschema =
      match `Assoc [("type", (`String "integer"))] with
      | `Assoc (("type", `String t)::[]) ->
          `Assoc [("type", (`List [`String t; `String "null"]))]
      | s ->
          `Assoc
            [("anyOf", (`List [s; `Assoc [("type", (`String "null"))]]))]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "opt" =
    print_schema opt_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": [ "integer", "null" ]
    }
    |}]]
type using_m = {
  m: Mod1.m_1 }[@@deriving jsonschema]
include
  struct
    let using_m_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("m",
                ((match Mod1.m_1_jsonschema with
                  | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                      `Assoc
                        (("$id",
                           (`String "urn:jsonschema:test.ml:1511:41955"))
                        :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                  | other -> other)))]));
        ("required", (`List [`String "m"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "using_m" =
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
    |}]]
type 'param2 poly2 =
  | C of 'param2 [@@deriving jsonschema ~variant_as_string]
include
  struct
    let poly2_jsonschema _param2 =
      `Assoc [("anyOf", (`List [`Assoc [("const", (`String "C"))]]))]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly2" =
    print_schema (poly2_jsonschema int_jsonschema);
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "C" } ]
    } |}]]
type tuple_with_variant = (int * [ `A  | `B [@name "second_cstr"]])[@@deriving
                                                                    jsonschema]
include
  struct
    let tuple_with_variant_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("prefixItems",
          (`List
             [`Assoc [("type", (`String "integer"))];
             `Assoc
               [("anyOf",
                  (`List
                     [`Assoc
                        [("type", (`String "array"));
                        ("prefixItems",
                          (`List [`Assoc [("const", (`String "A"))]]));
                        ("unevaluatedItems", (`Bool false));
                        ("minItems", (`Int 1));
                        ("maxItems", (`Int 1))];
                     `Assoc
                       [("type", (`String "array"));
                       ("prefixItems",
                         (`List [`Assoc [("const", (`String "second_cstr"))]]));
                       ("unevaluatedItems", (`Bool false));
                       ("minItems", (`Int 1));
                       ("maxItems", (`Int 1))]]))]]));
        ("unevaluatedItems", (`Bool false));
        ("minItems", (`Int 2));
        ("maxItems", (`Int 2))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "tuple_with_variant" =
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
    |}]]
type player_scores =
  {
  player: string ;
  scores: numbers [@ref "numbers"][@key "scores_ref"]}[@@deriving jsonschema]
include
  struct
    let player_scores_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("scores_ref",
                (`Assoc [("$ref", (`String "#/$defs/numbers"))]));
             ("player", (`Assoc [("type", (`String "string"))]))]));
        ("required", (`List [`String "scores_ref"; `String "player"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "player_scores" =
    print_schema ~id:"https://ahrefs.com/schemas/player_scores"
      ~title:"Player scores" ~description:"Object representing player scores"
      ~definitions:[("numbers", numbers_jsonschema)] player_scores_jsonschema;
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
    |}]]
type address = {
  street: string ;
  city: string ;
  zip: string }[@@deriving jsonschema]
include
  struct
    let address_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("zip", (`Assoc [("type", (`String "string"))]));
             ("city", (`Assoc [("type", (`String "string"))]));
             ("street", (`Assoc [("type", (`String "string"))]))]));
        ("required",
          (`List [`String "zip"; `String "city"; `String "street"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t = {
  name: string ;
  age: int ;
  email: string option ;
  address: address }[@@deriving jsonschema]
include
  struct
    let t_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("address",
                ((match address_jsonschema with
                  | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                      `Assoc
                        (("$id",
                           (`String "urn:jsonschema:test.ml:1633:45167"))
                        :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                  | other -> other)));
             ("email",
               ((match `Assoc [("type", (`String "string"))] with
                 | `Assoc (("type", `String t)::[]) ->
                     `Assoc [("type", (`List [`String t; `String "null"]))]
                 | s ->
                     `Assoc
                       [("anyOf",
                          (`List [s; `Assoc [("type", (`String "null"))]]))])));
             ("age", (`Assoc [("type", (`String "integer"))]));
             ("name", (`Assoc [("type", (`String "string"))]))]));
        ("required",
          (`List
             [`String "address";
             `String "email";
             `String "age";
             `String "name"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t" =
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
    |}]]
type tt =
  {
  name: string ;
  age: int ;
  email: string option ;
  home_address: address [@ref "shared_address"];
  work_address: address [@ref "shared_address"];
  retreat_address: address [@ref "shared_address"]}[@@deriving jsonschema]
include
  struct
    let tt_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("retreat_address",
                (`Assoc [("$ref", (`String "#/$defs/shared_address"))]));
             ("work_address",
               (`Assoc [("$ref", (`String "#/$defs/shared_address"))]));
             ("home_address",
               (`Assoc [("$ref", (`String "#/$defs/shared_address"))]));
             ("email",
               ((match `Assoc [("type", (`String "string"))] with
                 | `Assoc (("type", `String t)::[]) ->
                     `Assoc [("type", (`List [`String t; `String "null"]))]
                 | s ->
                     `Assoc
                       [("anyOf",
                          (`List [s; `Assoc [("type", (`String "null"))]]))])));
             ("age", (`Assoc [("type", (`String "integer"))]));
             ("name", (`Assoc [("type", (`String "string"))]))]));
        ("required",
          (`List
             [`String "retreat_address";
             `String "work_address";
             `String "home_address";
             `String "email";
             `String "age";
             `String "name"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "tt" =
    print_schema ~definitions:[("shared_address", address_jsonschema)]
      tt_jsonschema;
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
    |}]]
type c = char[@@deriving jsonschema]
include
  struct
    let c_jsonschema =
      `Assoc
        [("type", (`String "string"));
        ("minLength", (`Int 1));
        ("maxLength", (`Int 1))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "c" =
    print_schema c_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "string",
      "minLength": 1,
      "maxLength": 1
    } |}]]
type variant_inline_record =
  | A of {
  a: int } 
  | B of {
  b: string } [@@deriving jsonschema ~variant_as_string]
include
  struct
    let variant_inline_record_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "A"))];
              `Assoc [("const", (`String "B"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "variant_inline_record" =
    print_schema variant_inline_record_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "A" }, { "const": "B" } ]
    }
    |}]]
type inline_record_with_extra_fields =
  | User of {
  name: string ;
  email: string } [@jsonschema.allow_extra_fields ]
  | Guest of {
  ip: string } [@@deriving jsonschema]
include
  struct
    let inline_record_with_extra_fields_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "User"))];
                      `Assoc
                        [("type", (`String "object"));
                        ("properties",
                          (`Assoc
                             [("email",
                                (`Assoc [("type", (`String "string"))]));
                             ("name",
                               (`Assoc [("type", (`String "string"))]))]));
                        ("required",
                          (`List [`String "email"; `String "name"]));
                        ("additionalProperties", (`Bool true))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 2));
                 ("maxItems", (`Int 2))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List
                     [`Assoc [("const", (`String "Guest"))];
                     `Assoc
                       [("type", (`String "object"));
                       ("properties",
                         (`Assoc
                            [("ip", (`Assoc [("type", (`String "string"))]))]));
                       ("required", (`List [`String "ip"]));
                       ("additionalProperties", (`Bool false))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 2));
                ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "inline_record_with_extra_fields" =
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
    |}]]
type variant_with_payload =
  | A of int 
  | B 
  | C of int * string 
  | D of (int * string * bool) [@@deriving jsonschema ~variant_as_string]
include
  struct
    let variant_with_payload_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "A"))];
              `Assoc [("const", (`String "B"))];
              `Assoc [("const", (`String "C"))];
              `Assoc [("const", (`String "D"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "variant_with_payload" =
    print_schema variant_with_payload_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        { "const": "A" }, { "const": "B" }, { "const": "C" }, { "const": "D" }
      ]
    }
    |}]]
type t1 =
  | Typ 
  | Class of string [@@deriving jsonschema]
include
  struct
    let t1_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List [`Assoc [("const", (`String "Typ"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 1));
                 ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List
                     [`Assoc [("const", (`String "Class"))];
                     `Assoc [("type", (`String "string"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 2));
                ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t1" =
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
    |}]]
type t2 =
  | Typ 
  | Class of string [@@deriving jsonschema ~variant_as_string]
include
  struct
    let t2_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "Typ"))];
              `Assoc [("const", (`String "Class"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t2" =
    print_schema t2_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Typ" }, { "const": "Class" } ]
    }
    |}]]
type t3 =
  | Typ [@name "type"]
  | Class of string [@name "class"][@@deriving jsonschema]
include
  struct
    let t3_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List [`Assoc [("const", (`String "type"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 1));
                 ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List
                     [`Assoc [("const", (`String "class"))];
                     `Assoc [("type", (`String "string"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 2));
                ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t3" =
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
    |}]]
type t4 = (int * string)[@@deriving jsonschema]
include
  struct
    let t4_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("prefixItems",
          (`List
             [`Assoc [("type", (`String "integer"))];
             `Assoc [("type", (`String "string"))]]));
        ("unevaluatedItems", (`Bool false));
        ("minItems", (`Int 2));
        ("maxItems", (`Int 2))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t4" =
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
    } |}]]
type t5 = [ `A of (int * string * bool) ][@@deriving jsonschema]
include
  struct
    let t5_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "A"))];
                      `Assoc [("type", (`String "integer"))];
                      `Assoc [("type", (`String "string"))];
                      `Assoc [("type", (`String "boolean"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 4));
                 ("maxItems", (`Int 4))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t5" =
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
    |}]]
type t6 = [ `A of ((int * string * bool) * float) ][@@deriving jsonschema]
include
  struct
    let t6_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "A"))];
                      `Assoc
                        [("type", (`String "array"));
                        ("prefixItems",
                          (`List
                             [`Assoc [("type", (`String "integer"))];
                             `Assoc [("type", (`String "string"))];
                             `Assoc [("type", (`String "boolean"))]]));
                        ("unevaluatedItems", (`Bool false));
                        ("minItems", (`Int 3));
                        ("maxItems", (`Int 3))];
                      `Assoc [("type", (`String "number"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 3));
                 ("maxItems", (`Int 3))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t6" =
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
    |}]]
type t7 =
  | A of int * string * bool [@@deriving jsonschema]
include
  struct
    let t7_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "A"))];
                      `Assoc [("type", (`String "integer"))];
                      `Assoc [("type", (`String "string"))];
                      `Assoc [("type", (`String "boolean"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 4));
                 ("maxItems", (`Int 4))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t7" =
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
    |}]]
type t8 =
  | A of (int * string * bool) [@@deriving jsonschema]
include
  struct
    let t8_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "A"))];
                      `Assoc
                        [("type", (`String "array"));
                        ("prefixItems",
                          (`List
                             [`Assoc [("type", (`String "integer"))];
                             `Assoc [("type", (`String "string"))];
                             `Assoc [("type", (`String "boolean"))]]));
                        ("unevaluatedItems", (`Bool false));
                        ("minItems", (`Int 3));
                        ("maxItems", (`Int 3))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 2));
                 ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t8" =
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
    |}]]
type t9 =
  | A of (int * string * bool) * float [@@deriving jsonschema]
include
  struct
    let t9_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "A"))];
                      `Assoc
                        [("type", (`String "array"));
                        ("prefixItems",
                          (`List
                             [`Assoc [("type", (`String "integer"))];
                             `Assoc [("type", (`String "string"))];
                             `Assoc [("type", (`String "boolean"))]]));
                        ("unevaluatedItems", (`Bool false));
                        ("minItems", (`Int 3));
                        ("maxItems", (`Int 3))];
                      `Assoc [("type", (`String "number"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 3));
                 ("maxItems", (`Int 3))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t9" =
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
    |}]]
type t10 = [ `A of (int * string * bool) ][@@deriving jsonschema]
include
  struct
    let t10_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "A"))];
                      `Assoc [("type", (`String "integer"))];
                      `Assoc [("type", (`String "string"))];
                      `Assoc [("type", (`String "boolean"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 4));
                 ("maxItems", (`Int 4))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t10" =
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
    |}]]
type t11 = [ `B of (int * string * bool) ][@@deriving
                                            jsonschema
                                              ~polymorphic_variant_tuple]
include
  struct
    let t11_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "B"))];
                      `Assoc
                        [("type", (`String "array"));
                        ("prefixItems",
                          (`List
                             [`Assoc [("type", (`String "integer"))];
                             `Assoc [("type", (`String "string"))];
                             `Assoc [("type", (`String "boolean"))]]));
                        ("unevaluatedItems", (`Bool false));
                        ("minItems", (`Int 3));
                        ("maxItems", (`Int 3))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 2));
                 ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t11" =
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
    |}]]
type obj2 = {
  x: int }[@@deriving jsonschema][@@jsonschema.allow_extra_fields ]
include
  struct
    let obj2_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc [("x", (`Assoc [("type", (`String "integer"))]))]));
        ("required", (`List [`String "x"]));
        ("additionalProperties", (`Bool true))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type obj1 = {
  obj2: obj2 }[@@deriving jsonschema]
include
  struct
    let obj1_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("obj2",
                ((match obj2_jsonschema with
                  | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                      `Assoc
                        (("$id",
                           (`String "urn:jsonschema:test.ml:2107:56585"))
                        :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                  | other -> other)))]));
        ("required", (`List [`String "obj2"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested_obj = {
  obj1: obj1 }[@@deriving jsonschema][@@allow_extra_fields ]
include
  struct
    let nested_obj_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("obj1",
                ((match obj1_jsonschema with
                  | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                      `Assoc
                        (("$id",
                           (`String "urn:jsonschema:test.ml:2108:56643"))
                        :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                  | other -> other)))]));
        ("required", (`List [`String "obj1"]));
        ("additionalProperties", (`Bool true))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "nested_obj" =
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
    |}]]
open Melange_json.Primitives
type x_without_extra = {
  x: int }[@@deriving (json, jsonschema)][@@allow_extra_fields ]
include
  struct
    [%%ocaml.error
      "Ppxlib.Deriving: 'json' is not a supported type deriving generator"]
    let x_without_extra_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc [("x", (`Assoc [("type", (`String "integer"))]))]));
        ("required", (`List [`String "x"]));
        ("additionalProperties", (`Bool true))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type x_with_extra = {
  x: int ;
  y: int }[@@deriving (json, jsonschema)][@@allow_extra_fields ]
include
  struct
    [%%ocaml.error
      "Ppxlib.Deriving: 'json' is not a supported type deriving generator"]
    let x_with_extra_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("y", (`Assoc [("type", (`String "integer"))]));
             ("x", (`Assoc [("type", (`String "integer"))]))]));
        ("required", (`List [`String "y"; `String "x"]));
        ("additionalProperties", (`Bool true))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "extra_fields" =
    let _check_deseralization_ok =
      ({ x = 1; y = 1 } |> x_with_extra_to_json) |> x_without_extra_of_json in
    print_schema x_without_extra_jsonschema;
    [%expect
      "\n {\n   \"$schema\": \"https://json-schema.org/draft/2020-12/schema\",\n   \"type\": \"object\",\n   \"properties\": { \"x\": { \"type\": \"integer\" } },\n   \"required\": [ \"x\" ],\n   \"additionalProperties\": true\n }\n "]]
type 'url generic_link_traffic = {
  title: string option ;
  url: 'url }[@@deriving jsonschema]
include
  struct
    let generic_link_traffic_jsonschema url =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("url", url);
             ("title",
               ((match `Assoc [("type", (`String "string"))] with
                 | `Assoc (("type", `String t)::[]) ->
                     `Assoc [("type", (`List [`String t; `String "null"]))]
                 | s ->
                     `Assoc
                       [("anyOf",
                          (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
        ("required", (`List [`String "url"; `String "title"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "parameterized_record" =
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
    |}]]
type string_link_traffic = string generic_link_traffic[@@deriving jsonschema]
include
  struct
    let string_link_traffic_jsonschema =
      match generic_link_traffic_jsonschema
              (`Assoc [("type", (`String "string"))])
      with
      | `Assoc pairs when List.mem_assoc "$defs" pairs ->
          `Assoc (("$id", (`String "urn:jsonschema:test.ml:2182:58623")) ::
            (List.filter (fun (k, _) -> k <> "$id") pairs))
      | other -> other[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "instantiated_parameterized_record" =
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
    |}]]
type 'a poly_variant =
  | A 
  | B of 'a [@@deriving jsonschema]
include
  struct
    let poly_variant_jsonschema a =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems", (`List [`Assoc [("const", (`String "A"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 1));
                 ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List [`Assoc [("const", (`String "B"))]; a]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 2));
                ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "parameterized_variant" =
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
    } |}]]
type ('a, 'b) multi_param = {
  first: 'a ;
  second: 'b ;
  label: string }[@@deriving jsonschema]
include
  struct
    let multi_param_jsonschema a b =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("label", (`Assoc [("type", (`String "string"))]));
             ("second", b);
             ("first", a)]));
        ("required",
          (`List [`String "label"; `String "second"; `String "first"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "multi_param" =
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
    } |}]]
type 'a param_list = 'a list[@@deriving jsonschema]
include
  struct
    let param_list_jsonschema a =
      `Assoc [("type", (`String "array")); ("items", a)][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "parameterized_abstract" =
    print_schema (param_list_jsonschema string_jsonschema);
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "array",
      "items": { "type": "string" }
    } |}]]
type ('a, 'b) either =
  | Left of 'a 
  | Right of 'b [@@deriving jsonschema]
include
  struct
    let either_jsonschema a b =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List [`Assoc [("const", (`String "Left"))]; a]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 2));
                 ("maxItems", (`Int 2))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List [`Assoc [("const", (`String "Right"))]; b]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 2));
                ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "multi_param_variant" =
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
    |}]]
type ('a, 'b) either_alias = ('a, 'b) either[@@deriving jsonschema]
include
  struct
    let either_alias_jsonschema a b =
      match either_jsonschema a b with
      | `Assoc pairs when List.mem_assoc "$defs" pairs ->
          `Assoc (("$id", (`String "urn:jsonschema:test.ml:2294:61412")) ::
            (List.filter (fun (k, _) -> k <> "$id") pairs))
      | other -> other[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "multi_param_abstract" =
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
    |}]]
type ('a, 'b) direction =
  | North 
  | South [@@deriving jsonschema ~variant_as_string]
include
  struct
    let direction_jsonschema _a _b =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "North"))];
              `Assoc [("const", (`String "South"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "multi_param_variant_as_string" =
    print_schema (direction_jsonschema int_jsonschema string_jsonschema);
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "North" }, { "const": "South" } ]
    }
    |}]]
type nullable_fields =
  {
  plain: string option ;
  drop_simple: string option [@jsonschema.option ];
  drop_complex: int list option [@jsonschema.option ]}[@@deriving jsonschema]
include
  struct
    let nullable_fields_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("drop_complex",
                ((match `Assoc
                          [("type", (`String "array"));
                          ("items", (`Assoc [("type", (`String "integer"))]))]
                  with
                  | `Assoc (("type", `String t)::[]) ->
                      `Assoc [("type", (`List [`String t; `String "null"]))]
                  | s ->
                      `Assoc
                        [("anyOf",
                           (`List [s; `Assoc [("type", (`String "null"))]]))])));
             ("drop_simple",
               ((match `Assoc [("type", (`String "string"))] with
                 | `Assoc (("type", `String t)::[]) ->
                     `Assoc [("type", (`List [`String t; `String "null"]))]
                 | s ->
                     `Assoc
                       [("anyOf",
                          (`List [s; `Assoc [("type", (`String "null"))]]))])));
             ("plain",
               ((match `Assoc [("type", (`String "string"))] with
                 | `Assoc (("type", `String t)::[]) ->
                     `Assoc [("type", (`List [`String t; `String "null"]))]
                 | s ->
                     `Assoc
                       [("anyOf",
                          (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
        ("required", (`List [`String "plain"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "nullable_fields" =
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
    |}]]
type composing_type = string
let composing_type_jsonschema =
  `Assoc
    [("type", (`String "string")); ("description", (`String "A string"))]
type composing_record =
  {
  composing_type: composing_type option [@jsonschema.option ]}[@@deriving
                                                                jsonschema]
include
  struct
    let composing_record_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("composing_type",
                ((match match composing_type_jsonschema with
                        | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                            `Assoc
                              (("$id",
                                 (`String "urn:jsonschema:test.ml:2368:63427"))
                              ::
                              (List.filter (fun (k, _) -> k <> "$id") pairs))
                        | other -> other
                  with
                  | `Assoc (("type", `String t)::[]) ->
                      `Assoc [("type", (`List [`String t; `String "null"]))]
                  | s ->
                      `Assoc
                        [("anyOf",
                           (`List [s; `Assoc [("type", (`String "null"))]]))])))]));
        ("required", (`List []));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "nullable_option_composing" =
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
    |}]]
type 'a grade' =
  | A of 'a 
  | B of ('a grade' * 'a grade') 
  | C 
type 'a grade = 'a grade' =
  | A of 'a 
  | B of ('a grade * 'a grade) 
  | C [@@deriving jsonschema]
include
  struct
    let grade_jsonschema a =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List [`Assoc [("const", (`String "A"))]; a]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "B"))];
                       `Assoc
                         [("type", (`String "array"));
                         ("prefixItems",
                           (`List
                              [`Assoc [("$ref", (`String "#/$defs/grade"))];
                              `Assoc [("$ref", (`String "#/$defs/grade"))]]));
                         ("unevaluatedItems", (`Bool false));
                         ("minItems", (`Int 2));
                         ("maxItems", (`Int 2))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List [`Assoc [("const", (`String "C"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 1));
                  ("maxItems", (`Int 1))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:grade"));
        ("$defs", (`Assoc (("grade", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/grade"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "grade" =
    print_schema (grade_jsonschema int_jsonschema);
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:grade",
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
    |}]]
type self_ref = {
  children: self_ref list }[@@deriving jsonschema]
include
  struct
    let self_ref_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("children",
                  (`Assoc
                     [("type", (`String "array"));
                     ("items",
                       (`Assoc [("$ref", (`String "#/$defs/self_ref"))]))]))]));
          ("required", (`List [`String "children"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:self_ref"));
        ("$defs", (`Assoc (("self_ref", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/self_ref"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type two_self_refs = {
  a: self_ref ;
  b: self_ref }[@@deriving jsonschema]
include
  struct
    let two_self_refs_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("b",
                ((match self_ref_jsonschema with
                  | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                      `Assoc
                        (("$id",
                           (`String "urn:jsonschema:test.ml:2456:65714"))
                        :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                  | other -> other)));
             ("a",
               ((match self_ref_jsonschema with
                 | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                     `Assoc
                       (("$id",
                          (`String "urn:jsonschema:test.ml:2455:65698"))
                       :: (List.filter (fun (k, _) -> k <> "$id") pairs))
                 | other -> other)))]));
        ("required", (`List [`String "b"; `String "a"]));
        ("additionalProperties", (`Bool false))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "no_duplicate_id_when_recursive_type_used_twice" =
    print_schema two_self_refs_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": {
        "b": {
          "$id": "urn:jsonschema:test/test.ml:2456:65714",
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
          "$id": "urn:jsonschema:test/test.ml:2455:65698",
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
    |}]]
type ('atom, 'group_atom) filter =
  | Atom of 'atom 
  | Group of ('atom, 'group_atom) filter list * 'group_atom [@@deriving
                                                              jsonschema]
include
  struct
    let filter_jsonschema atom group_atom =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List [`Assoc [("const", (`String "Atom"))]; atom]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Group"))];
                       `Assoc
                         [("type", (`String "array"));
                         ("items",
                           (`Assoc [("$ref", (`String "#/$defs/filter"))]))];
                       group_atom]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 3));
                  ("maxItems", (`Int 3))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:filter"));
        ("$defs", (`Assoc (("filter", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/filter"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('atom, 'group_atom) bool_filter =
  | BoolAtom of ('atom, 'group_atom) filter 
  | BoolFilterGroup of ('atom, 'group_atom) bool_filter list [@@deriving
                                                               jsonschema]
include
  struct
    let bool_filter_jsonschema atom group_atom =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "BoolAtom"))];
                        (match filter_jsonschema atom group_atom with
                         | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                             `Assoc
                               (("$id",
                                  (`String
                                     "urn:jsonschema:test.ml:2514:67360"))
                               ::
                               (List.filter (fun (k, _) -> k <> "$id") pairs))
                         | other -> other)]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "BoolFilterGroup"))];
                       `Assoc
                         [("type", (`String "array"));
                         ("items",
                           (`Assoc
                              [("$ref", (`String "#/$defs/bool_filter"))]))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:bool_filter"));
        ("$defs", (`Assoc (("bool_filter", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/bool_filter"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "polymorphic_recursive_ref" =
    print_schema (filter_jsonschema int_jsonschema string_jsonschema);
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:filter",
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
    |}]]
type 'a rec_wrapper =
  | RWrap of 'a 
  | RNested of 'a rec_wrapper [@@deriving jsonschema]
include
  struct
    let rec_wrapper_jsonschema a =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List [`Assoc [("const", (`String "RWrap"))]; a]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "RNested"))];
                       `Assoc [("$ref", (`String "#/$defs/rec_wrapper"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:rec_wrapper"));
        ("$defs", (`Assoc (("rec_wrapper", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/rec_wrapper"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type outer_rec =
  | ORLeaf of int 
  | ORNode of outer_rec rec_wrapper [@@deriving jsonschema]
include
  struct
    let outer_rec_jsonschema =
      let _ppx_eds = ref [] in
      let _ppx_body =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "ORLeaf"))];
                        `Assoc [("type", (`String "integer"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "ORNode"))];
                       (match rec_wrapper_jsonschema
                                (`Assoc
                                   [("$ref", (`String "#/$defs/outer_rec"))])
                        with
                        | `Assoc ppx_pairs ->
                            (match List.assoc_opt "$defs" ppx_pairs with
                             | Some (`Assoc ppx_defs) ->
                                 (_ppx_eds :=
                                    ((!_ppx_eds) @
                                       (List.filter
                                          (fun (n, _) ->
                                             not
                                               (List.mem_assoc n (!_ppx_eds)))
                                          ppx_defs));
                                  `Assoc
                                    (List.filter
                                       (fun (k, _) ->
                                          (k <> "$id") && (k <> "$defs"))
                                       ppx_pairs))
                             | _ -> `Assoc ppx_pairs)
                        | ppx_other -> ppx_other)]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      `Assoc
        [("$id", (`String "urn:jsonschema:outer_rec"));
        ("$defs", (`Assoc (("outer_rec", _ppx_body) :: (!_ppx_eds))));
        ("$ref", (`String "#/$defs/outer_rec"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "parametric_recursive_cross_ref" =
    print_schema outer_rec_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:outer_rec",
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
    |}]]
[%%expect_test
  let "polymorphic_recursive_ref_bool_filter" =
    print_schema (bool_filter_jsonschema int_jsonschema string_jsonschema);
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "$id": "urn:jsonschema:bool_filter",
      "$defs": {
        "bool_filter": {
          "anyOf": [
            {
              "type": "array",
              "prefixItems": [
                { "const": "BoolAtom" },
                {
                  "$id": "urn:jsonschema:test/test.ml:2505:67189",
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
    |}]]
