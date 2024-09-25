[@@@ocaml.warning "-37-69"]
let print_schema ?definitions  ?id  ?title  ?description  s =
  let s =
    Ppx_deriving_jsonschema_runtime.json_schema ?definitions ?id ?title
      ?description s in
  let () = print_endline (Yojson.Basic.pretty_to_string s) in ()
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
                  [`Assoc [("const", (`String "A"))];
                  `Assoc [("const", (`String "B"))]]))][@@warning "-32-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    [%%expect_test
      let "m_1" =
        print_schema m_1_jsonschema;
        [%expect
          {|
      {
        "$schema": "https://json-schema.org/draft/2020-12/schema",
        "anyOf": [ { "const": "A" }, { "const": "B" } ]
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
                      [`Assoc [("const", (`String "C"))];
                      `Assoc [("const", (`String "D"))]]))][@@warning
                                                             "-32-39"]
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
        [%%expect_test
          let "m_2" =
            print_schema m_2_jsonschema;
            [%expect
              {|
        {
          "$schema": "https://json-schema.org/draft/2020-12/schema",
          "anyOf": [ { "const": "C" }, { "const": "D" } ]
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
             [("m2", Mod1.Mod2.m_2_jsonschema); ("m", Mod1.m_1_jsonschema)]));
        ("required", (`List [`String "m2"; `String "m"]))][@@warning
                                                            "-32-39"]
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
        "m2": { "anyOf": [ { "const": "C" }, { "const": "D" } ] },
        "m": { "anyOf": [ { "const": "A" }, { "const": "B" } ] }
      },
      "required": [ "m2", "m" ]
    } |}]]
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
              [`Assoc [("const", (`String "Success"))];
              `Assoc [("const", (`String "Error"))];
              `Assoc [("const", (`String "skipped"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "kind" =
    print_schema kind_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [
        { "const": "Success" }, { "const": "Error" }, { "const": "skipped" }
      ]
    } |}]]
type kind_as_array =
  | Success 
  | Error 
  | Skipped [@name "skipped"][@@deriving jsonschema ~variant_as_array]
include
  struct
    let kind_as_array_jsonschema =
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
  let "kind_as_array" =
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
    } |}]]
type poly_kind = [ `Aaa  | `Bbb  | `Ccc [@name "ccc"]][@@deriving jsonschema]
include
  struct
    let poly_kind_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "Aaa"))];
              `Assoc [("const", (`String "Bbb"))];
              `Assoc [("const", (`String "ccc"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_kind" =
    print_schema poly_kind_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Aaa" }, { "const": "Bbb" }, { "const": "ccc" } ]
    } |}]]
type poly_kind_as_array = [ `Aaa  | `Bbb  | `Ccc [@name "ccc"]][@@deriving
                                                                 jsonschema
                                                                   ~variant_as_array]
include
  struct
    let poly_kind_as_array_jsonschema =
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
  let "poly_kind_as_array" =
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
    } |}]]
type poly_kind_with_payload =
  [ `Aaa of int  | `Bbb  | `Ccc of (string * bool) [@name "ccc"]][@@deriving
                                                                   jsonschema]
include
  struct
    let poly_kind_with_payload_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "Aaa"))];
              `Assoc [("const", (`String "Bbb"))];
              `Assoc [("const", (`String "ccc"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_kind_with_payload" =
    print_schema poly_kind_with_payload_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Aaa" }, { "const": "Bbb" }, { "const": "ccc" } ]
    } |}]]
type poly_kind_with_payload_as_array =
  [ `Aaa of int  | `Bbb  | `Ccc of (string * bool) [@name "ccc"]][@@deriving
                                                                   jsonschema
                                                                    ~variant_as_array]
include
  struct
    let poly_kind_with_payload_as_array_jsonschema =
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
                     `Assoc
                       [("type", (`String "array"));
                       ("prefixItems",
                         (`List
                            [`Assoc [("type", (`String "string"))];
                            `Assoc [("type", (`String "boolean"))]]));
                       ("unevaluatedItems", (`Bool false));
                       ("minItems", (`Int 2));
                       ("maxItems", (`Int 2))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 2));
                ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_kind_with_payload_as_array" =
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
    } |}]]
type poly_inherit = [ `New_one  | `Second_one of int  | poly_kind][@@deriving
                                                                    jsonschema]
include
  struct
    let poly_inherit_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "New_one"))];
              `Assoc [("const", (`String "Second_one"))];
              poly_kind_jsonschema]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_inherit" =
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
    } |}]]
type poly_inherit_as_array =
  [ `New_one  | `Second_one of int  | poly_kind_as_array][@@deriving
                                                           jsonschema
                                                             ~variant_as_array]
include
  struct
    let poly_inherit_as_array_jsonschema =
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
              poly_kind_as_array_jsonschema]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly_inherit_as_array" =
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
    } |}]]
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
                        [`Assoc [("const", (`String "Foo"))];
                        `Assoc [("const", (`String "Bar"))];
                        `Assoc [("const", (`String "Baz"))]]))]));
             ("l",
               (`Assoc
                  [("type", (`String "array"));
                  ("items", (`Assoc [("type", (`String "string"))]))]));
             ("a",
               (`Assoc
                  [("type", (`String "array"));
                  ("items", (`Assoc [("type", (`String "number"))]))]));
             ("opt_int", (`Assoc [("type", (`String "integer"))]));
             ("comment", (`Assoc [("type", (`String "string"))]));
             ("kind_f", kind_jsonschema);
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
             `String "comment";
             `String "kind_f";
             `String "date"]))][@@warning "-32-39"]
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
    } |}]]
type events = event list[@@deriving jsonschema]
include
  struct
    let events_jsonschema =
      `Assoc [("type", (`String "array")); ("items", event_jsonschema)]
      [@@warning "-32-39"]
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
    } |}]]
type eventss = event list list[@@deriving jsonschema]
include
  struct
    let eventss_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          (`Assoc [("type", (`String "array")); ("items", event_jsonschema)]))]
      [@@warning "-32-39"]
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
    } |}]]
type event_comment = (event * string)[@@deriving jsonschema]
include
  struct
    let event_comment_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("prefixItems",
          (`List [event_jsonschema; `Assoc [("type", (`String "string"))]]));
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
    } |}]]
type event_comments' = event_comment list[@@deriving jsonschema]
include
  struct
    let event_comments'_jsonschema =
      `Assoc
        [("type", (`String "array")); ("items", event_comment_jsonschema)]
      [@@warning "-32-39"]
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
    } |}]]
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
                  [event_jsonschema; `Assoc [("type", (`String "integer"))]]));
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
    } |}]]
type events_array = events array[@@deriving jsonschema]
include
  struct
    let events_array_jsonschema =
      `Assoc [("type", (`String "array")); ("items", events_jsonschema)]
      [@@warning "-32-39"]
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
    } |}]]
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
    let opt_jsonschema = `Assoc [("type", (`String "integer"))][@@warning
                                                                 "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "opt" =
    print_schema opt_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "integer"
    } |}]]
type using_m = {
  m: Mod1.m_1 }[@@deriving jsonschema]
include
  struct
    let using_m_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties", (`Assoc [("m", Mod1.m_1_jsonschema)]));
        ("required", (`List [`String "m"]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "using_m" =
    print_schema using_m_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "type": "object",
      "properties": { "m": { "anyOf": [ { "const": "A" }, { "const": "B" } ] } },
      "required": [ "m" ]
    } |}]]
type 'param2 poly2 =
  | C of 'param2 [@@deriving jsonschema]
include
  struct
    let poly2_jsonschema =
      `Assoc [("anyOf", (`List [`Assoc [("const", (`String "C"))]]))]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "poly2" =
    print_schema poly2_jsonschema;
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
                     [`Assoc [("const", (`String "A"))];
                     `Assoc [("const", (`String "second_cstr"))]]))]]));
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
        { "anyOf": [ { "const": "A" }, { "const": "second_cstr" } ] }
      ],
      "unevaluatedItems": false,
      "minItems": 2,
      "maxItems": 2
    } |}]]
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
        ("required", (`List [`String "scores_ref"; `String "player"]))]
      [@@warning "-32-39"]
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
      "required": [ "scores_ref", "player" ]
    } |}]]
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
          (`List [`String "zip"; `String "city"; `String "street"]))]
      [@@warning "-32-39"]
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
             [("address", address_jsonschema);
             ("email", (`Assoc [("type", (`String "string"))]));
             ("age", (`Assoc [("type", (`String "integer"))]));
             ("name", (`Assoc [("type", (`String "string"))]))]));
        ("required",
          (`List [`String "address"; `String "age"; `String "name"]))]
      [@@warning "-32-39"]
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
          "required": [ "zip", "city", "street" ]
        },
        "email": { "type": "string" },
        "age": { "type": "integer" },
        "name": { "type": "string" }
      },
      "required": [ "address", "age", "name" ]
    } |}]]
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
             ("email", (`Assoc [("type", (`String "string"))]));
             ("age", (`Assoc [("type", (`String "integer"))]));
             ("name", (`Assoc [("type", (`String "string"))]))]));
        ("required",
          (`List
             [`String "retreat_address";
             `String "work_address";
             `String "home_address";
             `String "age";
             `String "name"]))][@@warning "-32-39"]
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
    } |}]]
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
  b: string } [@@deriving jsonschema ~variant_as_array]
include
  struct
    let variant_inline_record_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "A"))];
                      `Assoc
                        [("type", (`String "object"));
                        ("properties",
                          (`Assoc
                             [("a", (`Assoc [("type", (`String "integer"))]))]));
                        ("required", (`List [`String "a"]))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 2));
                 ("maxItems", (`Int 2))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List
                     [`Assoc [("const", (`String "B"))];
                     `Assoc
                       [("type", (`String "object"));
                       ("properties",
                         (`Assoc
                            [("b", (`Assoc [("type", (`String "string"))]))]));
                       ("required", (`List [`String "b"]))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 2));
                ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "variant_inline_record" =
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
    } |}]]
type variant_with_payload =
  | A of int 
  | B 
  | C of int * string 
  | D of (int * string * bool) [@@deriving jsonschema ~variant_as_array]
include
  struct
    let variant_with_payload_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "A"))];
                      `Assoc [("type", (`String "integer"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 2));
                 ("maxItems", (`Int 2))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems", (`List [`Assoc [("const", (`String "B"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 1));
                ("maxItems", (`Int 1))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List
                     [`Assoc [("const", (`String "C"))];
                     `Assoc [("type", (`String "integer"))];
                     `Assoc [("type", (`String "string"))]]));
                ("unevaluatedItems", (`Bool false));
                ("minItems", (`Int 3));
                ("maxItems", (`Int 3))];
              `Assoc
                [("type", (`String "array"));
                ("prefixItems",
                  (`List
                     [`Assoc [("const", (`String "D"))];
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
  let "variant_with_payload" =
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
    } |}]]
type t1 =
  | Typ 
  | Class of string [@@deriving jsonschema]
include
  struct
    let t1_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "Typ"))];
              `Assoc [("const", (`String "Class"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t1" =
    print_schema t1_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "Typ" }, { "const": "Class" } ]
    } |}]]
type t2 =
  | Typ 
  | Class of string [@@deriving jsonschema ~variant_as_array]
include
  struct
    let t2_jsonschema =
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
  let "t2" =
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
    } |}]]
type t3 =
  | Typ [@name "type"]
  | Class of string [@name "class"][@@deriving jsonschema]
include
  struct
    let t3_jsonschema =
      `Assoc
        [("anyOf",
           (`List
              [`Assoc [("const", (`String "type"))];
              `Assoc [("const", (`String "class"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[%%expect_test
  let "t3" =
    print_schema t3_jsonschema;
    [%expect
      {|
    {
      "$schema": "https://json-schema.org/draft/2020-12/schema",
      "anyOf": [ { "const": "type" }, { "const": "class" } ]
    } |}]]
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
