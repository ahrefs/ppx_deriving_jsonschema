[@@@ocaml.warning "-37-69"]
open Melange_json.Primitives
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
          let ppx_eds = ref [] in
          let ppx_result =
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
                      ("maxItems", (`Int 1))]]))] in
          match !ppx_eds with
          | [] -> ppx_result
          | ppx_defs ->
              (match ppx_result with
               | `Assoc ppx_pairs ->
                   `Assoc (("$defs", (`Assoc ppx_defs)) ::
                     (List.filter
                        (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                        ppx_pairs))
               | other -> other)[@@warning "-32-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    module Mod2 =
      struct
        type m_2 =
          | C 
          | D [@@deriving jsonschema]
        include
          struct
            let m_2_jsonschema =
              let ppx_eds = ref [] in
              let ppx_result =
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
                          ("maxItems", (`Int 1))]]))] in
              match !ppx_eds with
              | [] -> ppx_result
              | ppx_defs ->
                  (match ppx_result with
                   | `Assoc ppx_pairs ->
                       `Assoc (("$defs", (`Assoc ppx_defs)) ::
                         (List.filter
                            (fun (k, _) ->
                               not (Stdlib.String.equal k "$defs")) ppx_pairs))
                   | other -> other)[@@warning "-32-39"]
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
      end
  end
type with_modules = {
  m: Mod1.m_1 ;
  m2: Mod1.Mod2.m_2 }[@@deriving jsonschema]
include
  struct
    let with_modules_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("m2",
                  ((match Mod1.Mod2.m_2_jsonschema with
                    | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                        `Assoc
                          (("$id", (`String "file://shared/cases.ml:21")) ::
                          (List.filter
                             (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                             pairs))
                    | other -> other)));
               ("m",
                 ((match Mod1.m_1_jsonschema with
                   | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                       `Assoc (("$id", (`String "file://shared/cases.ml:20"))
                         ::
                         (List.filter
                            (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                            pairs))
                   | other -> other)))]));
          ("required", (`List [`String "m2"; `String "m"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type kind =
  | Success 
  | Error 
  | Skipped [@name "skipped"][@@deriving jsonschema]
include
  struct
    let kind_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                  ("maxItems", (`Int 1))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type kind_as_string =
  | Success 
  | Error 
  | Skipped [@name "skipped"][@@deriving jsonschema ~variant_as_string]
include
  struct
    let kind_as_string_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc [("const", (`String "Success"))];
                `Assoc [("const", (`String "Error"))];
                `Assoc [("const", (`String "skipped"))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type poly_kind = [ `Aaa  | `Bbb  | `Ccc [@name "ccc"]][@@deriving jsonschema]
include
  struct
    let poly_kind_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                  ("maxItems", (`Int 1))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type poly_kind_as_string = [ `Aaa  | `Bbb  | `Ccc [@name "ccc"]][@@deriving
                                                                  jsonschema
                                                                    ~variant_as_string]
include
  struct
    let poly_kind_as_string_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc [("const", (`String "Aaa"))];
                `Assoc [("const", (`String "Bbb"))];
                `Assoc [("const", (`String "ccc"))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type poly_kind_with_payload =
  [ `Aaa of int  | `Bbb  | `Ccc of (string * bool) [@name "ccc"]][@@deriving
                                                                   jsonschema]
include
  struct
    let poly_kind_with_payload_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                  ("maxItems", (`Int 3))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type poly_kind_with_payload_as_string =
  [ `Aaa of int  | `Bbb  | `Ccc of (string * bool) [@name "ccc"]][@@deriving
                                                                   jsonschema
                                                                    ~variant_as_string]
include
  struct
    let poly_kind_with_payload_as_string_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc [("const", (`String "Aaa"))];
                `Assoc [("const", (`String "Bbb"))];
                `Assoc [("const", (`String "ccc"))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type poly_inherit = [ `New_one  | `Second_one of int  | poly_kind][@@deriving
                                                                    jsonschema]
include
  struct
    let poly_inherit_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                     `Assoc (("$id", (`String "file://shared/cases.ml:61"))
                       ::
                       (List.filter
                          (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                          pairs))
                 | other -> other)]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type poly_inherit_as_string =
  [ `New_one  | `Second_one of int  | poly_kind_as_string][@@deriving
                                                            jsonschema
                                                              ~variant_as_string]
include
  struct
    let poly_inherit_as_string_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc [("const", (`String "New_one"))];
                `Assoc [("const", (`String "Second_one"))];
                (match poly_kind_as_string_jsonschema with
                 | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                     `Assoc (("$id", (`String "file://shared/cases.ml:67"))
                       ::
                       (List.filter
                          (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                          pairs))
                 | other -> other)]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
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
      let ppx_eds = ref [] in
      let ppx_result =
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
                 (`Assoc
                    [("type", (`List [`String "integer"; `String "null"]))]));
               ("comment", (`Assoc [("type", (`String "string"))]));
               ("kind_f",
                 ((match kind_jsonschema with
                   | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                       `Assoc (("$id", (`String "file://shared/cases.ml:72"))
                         ::
                         (List.filter
                            (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                            pairs))
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
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type recursive_record = {
  a: int ;
  b: recursive_record list }[@@deriving jsonschema]
include
  struct
    let recursive_record_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_recursive_record =
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
        [("$defs",
           (`Assoc
              ([("recursive_record", ppx_body_recursive_record)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/recursive_record"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type recursive_variant =
  | A of recursive_variant 
  | B [@@deriving jsonschema]
include
  struct
    let recursive_variant_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_recursive_variant =
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
        [("$defs",
           (`Assoc
              ([("recursive_variant", ppx_body_recursive_variant)] @
                 (!ppx_eds))));
        ("$ref", (`String "#/$defs/recursive_variant"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type tree =
  | Leaf 
  | Node of {
  value: int ;
  left: tree ;
  right: tree } [@@deriving jsonschema]
include
  struct
    let tree_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_tree =
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
        [("$defs", (`Assoc ([("tree", ppx_body_tree)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/tree"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type non_recursive = {
  x: int ;
  y: string }[@@deriving jsonschema]
include
  struct
    let non_recursive_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("y", (`Assoc [("type", (`String "string"))]));
               ("x", (`Assoc [("type", (`String "integer"))]))]));
          ("required", (`List [`String "y"; `String "x"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type foo = {
  bar: bar option }
and bar = {
  foo: foo option }[@@deriving jsonschema]
include
  struct
    let foo_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_foo =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("bar",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/bar"))];
                           `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "bar"]));
          ("additionalProperties", (`Bool false))] in
      let ppx_body_bar =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("foo",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/foo"))];
                           `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "foo"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$defs",
           (`Assoc
              ([("foo", ppx_body_foo); ("bar", ppx_body_bar)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/foo"))][@@warning "-32-39"]
    let bar_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_foo =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("bar",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/bar"))];
                           `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "bar"]));
          ("additionalProperties", (`Bool false))] in
      let ppx_body_bar =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("foo",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/foo"))];
                           `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "foo"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$defs",
           (`Assoc
              ([("foo", ppx_body_foo); ("bar", ppx_body_bar)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/bar"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
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
      let ppx_eds = ref [] in
      let ppx_body_expr =
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
      let ppx_body_stmt =
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
                                 (`Assoc
                                    [("anyOf",
                                       (`List
                                          [`Assoc
                                             [("$ref",
                                                (`String "#/$defs/stmt"))];
                                          `Assoc [("type", (`String "null"))]]))]));
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
        [("$defs",
           (`Assoc
              ([("expr", ppx_body_expr); ("stmt", ppx_body_stmt)] @
                 (!ppx_eds))));
        ("$ref", (`String "#/$defs/expr"))][@@warning "-32-39"]
    let stmt_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_expr =
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
      let ppx_body_stmt =
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
                                 (`Assoc
                                    [("anyOf",
                                       (`List
                                          [`Assoc
                                             [("$ref",
                                                (`String "#/$defs/stmt"))];
                                          `Assoc [("type", (`String "null"))]]))]));
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
        [("$defs",
           (`Assoc
              ([("expr", ppx_body_expr); ("stmt", ppx_body_stmt)] @
                 (!ppx_eds))));
        ("$ref", (`String "#/$defs/stmt"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type alpha = {
  x: int }
and beta = {
  y: string }[@@deriving jsonschema]
include
  struct
    let alpha_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc [("x", (`Assoc [("type", (`String "integer"))]))]));
          ("required", (`List [`String "x"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
    let beta_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc [("y", (`Assoc [("type", (`String "string"))]))]));
          ("required", (`List [`String "y"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
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
      let ppx_eds = ref [] in
      let ppx_body_node_a =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/node_c"))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("b",
                 (`Assoc
                    [("anyOf",
                       (`List
                          [`Assoc [("$ref", (`String "#/$defs/node_b"))];
                          `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "c"; `String "b"]));
          ("additionalProperties", (`Bool false))] in
      let ppx_body_node_b =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/node_c"))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("a",
                 (`Assoc
                    [("anyOf",
                       (`List
                          [`Assoc [("$ref", (`String "#/$defs/node_a"))];
                          `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "c"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      let ppx_body_node_c =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("b",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/node_b"))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("a",
                 (`Assoc
                    [("anyOf",
                       (`List
                          [`Assoc [("$ref", (`String "#/$defs/node_a"))];
                          `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "b"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$defs",
           (`Assoc
              ([("node_a", ppx_body_node_a);
               ("node_b", ppx_body_node_b);
               ("node_c", ppx_body_node_c)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/node_a"))][@@warning "-32-39"]
    let node_b_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_node_a =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/node_c"))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("b",
                 (`Assoc
                    [("anyOf",
                       (`List
                          [`Assoc [("$ref", (`String "#/$defs/node_b"))];
                          `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "c"; `String "b"]));
          ("additionalProperties", (`Bool false))] in
      let ppx_body_node_b =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/node_c"))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("a",
                 (`Assoc
                    [("anyOf",
                       (`List
                          [`Assoc [("$ref", (`String "#/$defs/node_a"))];
                          `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "c"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      let ppx_body_node_c =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("b",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/node_b"))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("a",
                 (`Assoc
                    [("anyOf",
                       (`List
                          [`Assoc [("$ref", (`String "#/$defs/node_a"))];
                          `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "b"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$defs",
           (`Assoc
              ([("node_a", ppx_body_node_a);
               ("node_b", ppx_body_node_b);
               ("node_c", ppx_body_node_c)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/node_b"))][@@warning "-32-39"]
    let node_c_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_node_a =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/node_c"))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("b",
                 (`Assoc
                    [("anyOf",
                       (`List
                          [`Assoc [("$ref", (`String "#/$defs/node_b"))];
                          `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "c"; `String "b"]));
          ("additionalProperties", (`Bool false))] in
      let ppx_body_node_b =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("c",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/node_c"))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("a",
                 (`Assoc
                    [("anyOf",
                       (`List
                          [`Assoc [("$ref", (`String "#/$defs/node_a"))];
                          `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "c"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      let ppx_body_node_c =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("b",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc [("$ref", (`String "#/$defs/node_b"))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("a",
                 (`Assoc
                    [("anyOf",
                       (`List
                          [`Assoc [("$ref", (`String "#/$defs/node_a"))];
                          `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List [`String "b"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      `Assoc
        [("$defs",
           (`Assoc
              ([("node_a", ppx_body_node_a);
               ("node_b", ppx_body_node_b);
               ("node_c", ppx_body_node_c)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/node_c"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type recursive_tuple =
  | Leaf of int 
  | Branch of (recursive_tuple * recursive_tuple) [@@deriving jsonschema]
include
  struct
    let recursive_tuple_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_recursive_tuple =
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
        [("$defs",
           (`Assoc
              ([("recursive_tuple", ppx_body_recursive_tuple)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/recursive_tuple"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type int_tree = tree[@@deriving jsonschema]
include
  struct
    let int_tree_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        match tree_jsonschema with
        | `Assoc pairs when List.mem_assoc "$defs" pairs ->
            `Assoc (("$id", (`String "file://shared/cases.ml:140")) ::
              (List.filter (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                 pairs))
        | other -> other in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type events = event list[@@deriving jsonschema]
include
  struct
    let events_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "array"));
          ("items",
            ((match event_jsonschema with
              | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                  `Assoc (("$id", (`String "file://shared/cases.ml:141")) ::
                    (List.filter
                       (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                       pairs))
              | other -> other)))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type eventss = event list list[@@deriving jsonschema]
include
  struct
    let eventss_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "array"));
          ("items",
            (`Assoc
               [("type", (`String "array"));
               ("items",
                 ((match event_jsonschema with
                   | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                       `Assoc
                         (("$id", (`String "file://shared/cases.ml:142")) ::
                         (List.filter
                            (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                            pairs))
                   | other -> other)))]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type event_comment = (event * string)[@@deriving jsonschema]
include
  struct
    let event_comment_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "array"));
          ("prefixItems",
            (`List
               [(match event_jsonschema with
                 | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                     `Assoc (("$id", (`String "file://shared/cases.ml:143"))
                       ::
                       (List.filter
                          (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                          pairs))
                 | other -> other);
               `Assoc [("type", (`String "string"))]]));
          ("unevaluatedItems", (`Bool false));
          ("minItems", (`Int 2));
          ("maxItems", (`Int 2))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type event_comments' = event_comment list[@@deriving jsonschema]
include
  struct
    let event_comments'_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "array"));
          ("items",
            ((match event_comment_jsonschema with
              | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                  `Assoc (("$id", (`String "file://shared/cases.ml:144")) ::
                    (List.filter
                       (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                       pairs))
              | other -> other)))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type event_n = (event * int) list[@@deriving jsonschema]
include
  struct
    let event_n_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                            (("$id", (`String "file://shared/cases.ml:145"))
                            ::
                            (List.filter
                               (fun (k, _) ->
                                  not (Stdlib.String.equal k "$id")) pairs))
                      | other -> other);
                    `Assoc [("type", (`String "integer"))]]));
               ("unevaluatedItems", (`Bool false));
               ("minItems", (`Int 2));
               ("maxItems", (`Int 2))]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type events_array = events array[@@deriving jsonschema]
include
  struct
    let events_array_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "array"));
          ("items",
            ((match events_jsonschema with
              | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                  `Assoc (("$id", (`String "file://shared/cases.ml:146")) ::
                    (List.filter
                       (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                       pairs))
              | other -> other)))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type numbers = int list[@@deriving jsonschema]
include
  struct
    let numbers_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "array"));
          ("items", (`Assoc [("type", (`String "integer"))]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type opt = int option[@@deriving jsonschema]
include
  struct
    let opt_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc [("type", (`List [`String "integer"; `String "null"]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type using_m = {
  m: Mod1.m_1 }[@@deriving jsonschema]
include
  struct
    let using_m_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("m",
                  ((match Mod1.m_1_jsonschema with
                    | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                        `Assoc
                          (("$id", (`String "file://shared/cases.ml:149")) ::
                          (List.filter
                             (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                             pairs))
                    | other -> other)))]));
          ("required", (`List [`String "m"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'param2 poly2 =
  | C of 'param2 [@@deriving jsonschema ~variant_as_string]
include
  struct
    let poly2_jsonschema _param2 =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc [("anyOf", (`List [`Assoc [("const", (`String "C"))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type tuple_with_variant = (int * [ `A  | `B [@name "second_cstr"]])[@@deriving
                                                                    jsonschema]
include
  struct
    let tuple_with_variant_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                           (`List
                              [`Assoc [("const", (`String "second_cstr"))]]));
                         ("unevaluatedItems", (`Bool false));
                         ("minItems", (`Int 1));
                         ("maxItems", (`Int 1))]]))]]));
          ("unevaluatedItems", (`Bool false));
          ("minItems", (`Int 2));
          ("maxItems", (`Int 2))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type player_scores =
  {
  player: string ;
  scores: numbers [@ref "numbers"][@key "scores_ref"]}[@@deriving jsonschema]
include
  struct
    let player_scores_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("scores_ref",
                  (`Assoc [("$ref", (`String "#/$defs/numbers"))]));
               ("player", (`Assoc [("type", (`String "string"))]))]));
          ("required", (`List [`String "scores_ref"; `String "player"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type address = {
  street: string ;
  city: string ;
  zip: string }[@@deriving jsonschema]
include
  struct
    let address_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("zip", (`Assoc [("type", (`String "string"))]));
               ("city", (`Assoc [("type", (`String "string"))]));
               ("street", (`Assoc [("type", (`String "string"))]))]));
          ("required",
            (`List [`String "zip"; `String "city"; `String "street"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t = {
  name: string ;
  age: int ;
  email: string option ;
  address: address }[@@deriving jsonschema]
include
  struct
    let t_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("address",
                  ((match address_jsonschema with
                    | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                        `Assoc
                          (("$id", (`String "file://shared/cases.ml:167")) ::
                          (List.filter
                             (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                             pairs))
                    | other -> other)));
               ("email",
                 (`Assoc
                    [("type", (`List [`String "string"; `String "null"]))]));
               ("age", (`Assoc [("type", (`String "integer"))]));
               ("name", (`Assoc [("type", (`String "string"))]))]));
          ("required",
            (`List
               [`String "address";
               `String "email";
               `String "age";
               `String "name"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
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
      let ppx_eds = ref [] in
      let ppx_result =
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
                 (`Assoc
                    [("type", (`List [`String "string"; `String "null"]))]));
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
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type c = char[@@deriving jsonschema]
include
  struct
    let c_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "string"));
          ("minLength", (`Int 1));
          ("maxLength", (`Int 1))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type variant_inline_record =
  | A of {
  a: int } 
  | B of {
  b: string } [@@deriving jsonschema ~variant_as_string]
include
  struct
    let variant_inline_record_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc [("const", (`String "A"))];
                `Assoc [("const", (`String "B"))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type inline_record_with_extra_fields =
  | User of {
  name: string ;
  email: string } [@jsonschema.allow_extra_fields ]
  | Guest of {
  ip: string } [@@deriving jsonschema]
include
  struct
    let inline_record_with_extra_fields_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                              [("ip",
                                 (`Assoc [("type", (`String "string"))]))]));
                         ("required", (`List [`String "ip"]));
                         ("additionalProperties", (`Bool false))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type variant_with_payload =
  | A of int 
  | B 
  | C of int * string 
  | D of (int * string * bool) [@@deriving jsonschema ~variant_as_string]
include
  struct
    let variant_with_payload_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc [("const", (`String "A"))];
                `Assoc [("const", (`String "B"))];
                `Assoc [("const", (`String "C"))];
                `Assoc [("const", (`String "D"))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t1 =
  | Typ 
  | Class of string [@@deriving jsonschema]
include
  struct
    let t1_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t2 =
  | Typ 
  | Class of string [@@deriving jsonschema ~variant_as_string]
include
  struct
    let t2_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc [("const", (`String "Typ"))];
                `Assoc [("const", (`String "Class"))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t3 =
  | Typ [@name "type"]
  | Class of string [@name "class"][@@deriving jsonschema]
include
  struct
    let t3_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t4 = (int * string)[@@deriving jsonschema]
include
  struct
    let t4_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "array"));
          ("prefixItems",
            (`List
               [`Assoc [("type", (`String "integer"))];
               `Assoc [("type", (`String "string"))]]));
          ("unevaluatedItems", (`Bool false));
          ("minItems", (`Int 2));
          ("maxItems", (`Int 2))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t5 = [ `A of (int * string * bool) ][@@deriving jsonschema]
include
  struct
    let t5_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                   ("maxItems", (`Int 4))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t6 = [ `A of ((int * string * bool) * float) ][@@deriving jsonschema]
include
  struct
    let t6_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                   ("maxItems", (`Int 3))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t7 =
  | A of int * string * bool [@@deriving jsonschema]
include
  struct
    let t7_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                   ("maxItems", (`Int 4))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t8 =
  | A of (int * string * bool) [@@deriving jsonschema]
include
  struct
    let t8_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                   ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t9 =
  | A of (int * string * bool) * float [@@deriving jsonschema]
include
  struct
    let t9_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                   ("maxItems", (`Int 3))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t10 = [ `A of (int * string * bool) ][@@deriving jsonschema]
include
  struct
    let t10_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                   ("maxItems", (`Int 4))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type t11 = [ `B of (int * string * bool) ][@@deriving
                                            jsonschema
                                              ~polymorphic_variant_tuple]
include
  struct
    let t11_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                   ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type obj2 = {
  x: int }[@@deriving jsonschema][@@jsonschema.allow_extra_fields ]
include
  struct
    let obj2_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc [("x", (`Assoc [("type", (`String "integer"))]))]));
          ("required", (`List [`String "x"]));
          ("additionalProperties", (`Bool true))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type obj1 = {
  obj2: obj2 }[@@deriving jsonschema]
include
  struct
    let obj1_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("obj2",
                  ((match obj2_jsonschema with
                    | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                        `Assoc
                          (("$id", (`String "file://shared/cases.ml:218")) ::
                          (List.filter
                             (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                             pairs))
                    | other -> other)))]));
          ("required", (`List [`String "obj2"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nested_obj = {
  obj1: obj1 }[@@deriving jsonschema][@@allow_extra_fields ]
include
  struct
    let nested_obj_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("obj1",
                  ((match obj1_jsonschema with
                    | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                        `Assoc
                          (("$id", (`String "file://shared/cases.ml:219")) ::
                          (List.filter
                             (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                             pairs))
                    | other -> other)))]));
          ("required", (`List [`String "obj1"]));
          ("additionalProperties", (`Bool true))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type x_without_extra = {
  x: int }[@@deriving jsonschema][@@allow_extra_fields ]
include
  struct
    let x_without_extra_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc [("x", (`Assoc [("type", (`String "integer"))]))]));
          ("required", (`List [`String "x"]));
          ("additionalProperties", (`Bool true))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type x_with_extra = {
  x: int ;
  y: int }[@@deriving jsonschema][@@allow_extra_fields ]
include
  struct
    let x_with_extra_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("y", (`Assoc [("type", (`String "integer"))]));
               ("x", (`Assoc [("type", (`String "integer"))]))]));
          ("required", (`List [`String "y"; `String "x"]));
          ("additionalProperties", (`Bool true))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'url generic_link_traffic = {
  title: string option ;
  url: 'url }[@@deriving jsonschema]
include
  struct
    let generic_link_traffic_jsonschema url =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("url", url);
               ("title",
                 (`Assoc
                    [("type", (`List [`String "string"; `String "null"]))]))]));
          ("required", (`List [`String "url"; `String "title"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type string_link_traffic = string generic_link_traffic[@@deriving jsonschema]
include
  struct
    let string_link_traffic_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        match generic_link_traffic_jsonschema
                (`Assoc [("type", (`String "string"))])
        with
        | `Assoc pairs when List.mem_assoc "$defs" pairs ->
            `Assoc (("$id", (`String "file://shared/cases.ml:231")) ::
              (List.filter (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                 pairs))
        | other -> other in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'a poly_variant =
  | A 
  | B of 'a [@@deriving jsonschema]
include
  struct
    let poly_variant_jsonschema a =
      let ppx_eds = ref [] in
      let ppx_result =
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
                    (`List [`Assoc [("const", (`String "B"))]; a]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('a, 'b) multi_param = {
  first: 'a ;
  second: 'b ;
  label: string }[@@deriving jsonschema]
include
  struct
    let multi_param_jsonschema a b =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("label", (`Assoc [("type", (`String "string"))]));
               ("second", b);
               ("first", a)]));
          ("required",
            (`List [`String "label"; `String "second"; `String "first"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'a param_list = 'a list[@@deriving jsonschema]
include
  struct
    let param_list_jsonschema a =
      let ppx_eds = ref [] in
      let ppx_result = `Assoc [("type", (`String "array")); ("items", a)] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('a, 'b) either =
  | Left of 'a 
  | Right of 'b [@@deriving jsonschema]
include
  struct
    let either_jsonschema a b =
      let ppx_eds = ref [] in
      let ppx_result =
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
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('a, 'b) either_alias = ('a, 'b) either[@@deriving jsonschema]
include
  struct
    let either_alias_jsonschema a b =
      let ppx_eds = ref [] in
      let ppx_result =
        match either_jsonschema a b with
        | `Assoc pairs when List.mem_assoc "$defs" pairs ->
            `Assoc (("$id", (`String "file://shared/cases.ml:247")) ::
              (List.filter (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                 pairs))
        | other -> other in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('a, 'b) direction =
  | North 
  | South [@@deriving jsonschema ~variant_as_string]
include
  struct
    let direction_jsonschema _a _b =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc [("const", (`String "North"))];
                `Assoc [("const", (`String "South"))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type tool_params =
  {
  query: string [@jsonschema.description "The search query to execute"];
  max_results: int
    [@jsonschema.description "Maximum number of results to return"]}[@@deriving
                                                                    jsonschema]
include
  struct
    let tool_params_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("max_results",
                  (`Assoc
                     [("description",
                        (`String "Maximum number of results to return"));
                     ("type", (`String "integer"))]));
               ("query",
                 (`Assoc
                    [("description", (`String "The search query to execute"));
                    ("type", (`String "string"))]))]));
          ("required", (`List [`String "max_results"; `String "query"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type described_record =
  {
  name: string [@jsonschema.description "The user's full name"];
  age: int option
    [@jsonschema.option ][@jsonschema.description "The user's age"]}[@@deriving
                                                                    jsonschema]
[@@jsonschema.description "A user object"]
include
  struct
    let described_record_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("description", (`String "A user object"));
          ("type", (`String "object"));
          ("properties",
            (`Assoc
               [("age",
                  (`Assoc
                     [("description", (`String "The user's age"));
                     ("type", (`List [`String "integer"; `String "null"]))]));
               ("name",
                 (`Assoc
                    [("description", (`String "The user's full name"));
                    ("type", (`String "string"))]))]));
          ("required", (`List [`String "name"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_key_and_desc =
  {
  opt: int option
    [@key "opt_int"][@jsonschema.option ][@jsonschema.description
                                           "An optional integer"]}[@@deriving
                                                                    jsonschema]
include
  struct
    let with_key_and_desc_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("opt_int",
                  (`Assoc
                     [("description", (`String "An optional integer"));
                     ("type", (`List [`String "integer"; `String "null"]))]))]));
          ("required", (`List []));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type described_variant =
  | Plain [@jsonschema.description "No payload"]
  | With_int of int [@jsonschema.description "Single integer tag"]
  | Pair of string * int [@jsonschema.description "String and int"]
  | Description_value of ((string)[@jsonschema.description "A string value"]) 
[@@deriving jsonschema]
include
  struct
    let described_variant_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("description", (`String "No payload"));
                   ("type", (`String "array"));
                   ("prefixItems",
                     (`List [`Assoc [("const", (`String "Plain"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 1));
                   ("maxItems", (`Int 1))];
                `Assoc
                  [("description", (`String "Single integer tag"));
                  ("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "With_int"))];
                       `Assoc [("type", (`String "integer"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))];
                `Assoc
                  [("description", (`String "String and int"));
                  ("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Pair"))];
                       `Assoc [("type", (`String "string"))];
                       `Assoc [("type", (`String "integer"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 3));
                  ("maxItems", (`Int 3))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Description_value"))];
                       `Assoc
                         [("description", (`String "A string value"));
                         ("type", (`String "string"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type described_variant_inline_record =
  | Point of {
  x: int ;
  y: int } [@jsonschema.description "A 2D point"][@@deriving jsonschema]
include
  struct
    let described_variant_inline_record_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("description", (`String "A 2D point"));
                   ("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "Point"))];
                        `Assoc
                          [("type", (`String "object"));
                          ("properties",
                            (`Assoc
                               [("y",
                                  (`Assoc [("type", (`String "integer"))]));
                               ("x",
                                 (`Assoc [("type", (`String "integer"))]))]));
                          ("required", (`List [`String "y"; `String "x"]));
                          ("additionalProperties", (`Bool false))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type described_variant_string =
  | A [@jsonschema.description "First choice"]
  | B [@jsonschema.description "Second choice"][@@deriving
                                                 jsonschema
                                                   ~variant_as_string]
include
  struct
    let described_variant_string_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("description", (`String "First choice"));
                   ("const", (`String "A"))];
                `Assoc
                  [("description", (`String "Second choice"));
                  ("const", (`String "B"))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type doc_comment_record =
  {
  name: string [@ocaml.doc " The user's full name "];
  age: int [@ocaml.doc " The user's age "]}[@@ocaml.doc " A user object "]
[@@deriving jsonschema ~ocaml_doc]
include
  struct
    let doc_comment_record_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("description", (`String "A user object"));
          ("type", (`String "object"));
          ("properties",
            (`Assoc
               [("age",
                  (`Assoc
                     [("description", (`String "The user's age"));
                     ("type", (`String "integer"))]));
               ("name",
                 (`Assoc
                    [("description", (`String "The user's full name"));
                    ("type", (`String "string"))]))]));
          ("required", (`List [`String "age"; `String "name"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type doc_comment_disabled =
  {
  name: string [@ocaml.doc " The user's full name "]}[@@ocaml.doc
                                                       " A user object "]
[@@deriving jsonschema]
include
  struct
    let doc_comment_disabled_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc [("name", (`Assoc [("type", (`String "string"))]))]));
          ("required", (`List [`String "name"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type doc_comment_override =
  {
  field: string
    [@jsonschema.description "explicit wins"][@ocaml.doc " ocaml.doc loses "]}
[@@deriving jsonschema ~ocaml_doc]
include
  struct
    let doc_comment_override_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("field",
                  (`Assoc
                     [("description", (`String "explicit wins"));
                     ("type", (`String "string"))]))]));
          ("required", (`List [`String "field"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type doc_comment_variant =
  | Plain [@ocaml.doc " No payload "]
  | With_int of int [@ocaml.doc " Single integer tag "][@@deriving
                                                         jsonschema
                                                           ~ocaml_doc]
include
  struct
    let doc_comment_variant_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("description", (`String "No payload"));
                   ("type", (`String "array"));
                   ("prefixItems",
                     (`List [`Assoc [("const", (`String "Plain"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 1));
                   ("maxItems", (`Int 1))];
                `Assoc
                  [("description", (`String "Single integer tag"));
                  ("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "With_int"))];
                       `Assoc [("type", (`String "integer"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type doc_comment_core_type = ((string)[@ocaml.doc " A string alias "])
[@@deriving jsonschema ~ocaml_doc]
include
  struct
    let doc_comment_core_type_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("description", (`String "A string alias"));
          ("type", (`String "string"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type doc_attribute_alias = ((string)[@doc " Alias fallback "])[@@deriving
                                                                jsonschema
                                                                  ~ocaml_doc]
include
  struct
    let doc_attribute_alias_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("description", (`String "Alias fallback"));
          ("type", (`String "string"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[@@@ocamlformat "disable"]
type doc_comment_multiline =
  {
  name: string
    [@ocaml.doc
      " The user's full name.\n          Must be non-empty and under 100 characters. "]}
[@@deriving jsonschema ~ocaml_doc]
include
  struct
    let doc_comment_multiline_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("name",
                  (`Assoc
                     [("description",
                        (`String
                           "The user's full name.\n          Must be non-empty and under 100 characters."));
                     ("type", (`String "string"))]))]));
          ("required", (`List [`String "name"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
[@@@ocamlformat "enable"]
type doc_comment_poly_variant =
  [ `Plain [@ocaml.doc " No payload "]
  | `With_int of int [@ocaml.doc " Single integer tag "]][@@deriving
                                                           jsonschema
                                                             ~ocaml_doc]
include
  struct
    let doc_comment_poly_variant_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("description", (`String "No payload"));
                   ("type", (`String "array"));
                   ("prefixItems",
                     (`List [`Assoc [("const", (`String "Plain"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 1));
                   ("maxItems", (`Int 1))];
                `Assoc
                  [("description", (`String "Single integer tag"));
                  ("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "With_int"))];
                       `Assoc [("type", (`String "integer"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type doc_comment_multiple =
  ((string)[@ocaml.doc " first block "][@ocaml.doc " second block "][@doc
                                                                    " third block "])
[@@deriving jsonschema ~ocaml_doc]
include
  struct
    let doc_comment_multiple_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("description",
             (`String "first block\n\nsecond block\n\nthird block"));
          ("type", (`String "string"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type doc_comment_poly_variant_override =
  [
    `Tagged
      [@jsonschema.description "explicit wins"][@ocaml.doc
                                                 " ocaml.doc loses "]]
[@@deriving jsonschema ~ocaml_doc]
include
  struct
    let doc_comment_poly_variant_override_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("description", (`String "explicit wins"));
                   ("type", (`String "array"));
                   ("prefixItems",
                     (`List [`Assoc [("const", (`String "Tagged"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 1));
                   ("maxItems", (`Int 1))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type computation_result =
  | Ok 
  | Err of string [@@deriving jsonschema][@@jsonschema.description
                                           "Either success or an error message string"]
include
  struct
    let computation_result_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("description",
             (`String "Either success or an error message string"));
          ("anyOf",
            (`List
               [`Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List [`Assoc [("const", (`String "Ok"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 1));
                  ("maxItems", (`Int 1))];
               `Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "Err"))];
                      `Assoc [("type", (`String "string"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 2));
                 ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type nullable_fields =
  {
  plain: string option ;
  drop_simple: string option [@jsonschema.option ];
  drop_complex: int list option [@jsonschema.option ]}[@@deriving jsonschema]
include
  struct
    let nullable_fields_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("drop_complex",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [`Assoc
                              [("type", (`String "array"));
                              ("items",
                                (`Assoc [("type", (`String "integer"))]))];
                           `Assoc [("type", (`String "null"))]]))]));
               ("drop_simple",
                 (`Assoc
                    [("type", (`List [`String "string"; `String "null"]))]));
               ("plain",
                 (`Assoc
                    [("type", (`List [`String "string"; `String "null"]))]))]));
          ("required", (`List [`String "plain"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type melange_json_defaults =
  {
  required_value: int ;
  option_value: string option [@option ];
  option_default_none: string [@default None];
  default_none: string [@default None];
  default_value: string [@default "-"];
  dropped_option: int option [@option ][@drop_default ];
  dropped_default: int list [@default []][@drop_default ];
  custom_drop_default: float [@default 0.0][@drop_default Float.equal]}
[@@deriving jsonschema]
include
  struct
    let melange_json_defaults_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("custom_drop_default",
                  (`Assoc
                     [("default", (((fun x -> `Float x)) 0.0));
                     ("type", (`String "number"))]));
               ("dropped_default",
                 (`Assoc
                    [("default", (`List []));
                    ("type", (`String "array"));
                    ("items", (`Assoc [("type", (`String "integer"))]))]));
               ("dropped_option",
                 (`Assoc
                    [("type", (`List [`String "integer"; `String "null"]))]));
               ("default_value",
                 (`Assoc
                    [("default", (((fun x -> `String x)) "-"));
                    ("type", (`String "string"))]));
               ("default_none",
                 (`Assoc [("default", `Null); ("type", (`String "string"))]));
               ("option_default_none",
                 (`Assoc [("default", `Null); ("type", (`String "string"))]));
               ("option_value",
                 (`Assoc
                    [("type", (`List [`String "string"; `String "null"]))]));
               ("required_value", (`Assoc [("type", (`String "integer"))]))]));
          ("required", (`List [`String "required_value"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
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
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("composing_type",
                  (`Assoc
                     [("anyOf",
                        (`List
                           [(match composing_type_jsonschema with
                             | `Assoc pairs when List.mem_assoc "$defs" pairs
                                 ->
                                 `Assoc
                                   (("$id",
                                      (`String "file://shared/cases.ml:361"))
                                   ::
                                   (List.filter
                                      (fun (k, _) ->
                                         not (Stdlib.String.equal k "$id"))
                                      pairs))
                             | other -> other);
                           `Assoc [("type", (`String "null"))]]))]))]));
          ("required", (`List []));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_format = string[@@jsonschema.format "date-time"][@@deriving
                                                            jsonschema]
include
  struct
    let with_format_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("format", (`String "date-time")); ("type", (`String "string"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_format_record =
  {
  with_format: string [@jsonschema.format "date-time"]}[@@deriving
                                                         jsonschema]
include
  struct
    let with_format_record_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("with_format",
                  (`Assoc
                     [("format", (`String "date-time"));
                     ("type", (`String "string"))]))]));
          ("required", (`List [`String "with_format"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_format_variant =
  | A of
  ((string)[@jsonschema.format "date-time"][@jsonschema.description
                                             "A date-time string"])
  
  | B [@@deriving jsonschema]
include
  struct
    let with_format_variant_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "A"))];
                        `Assoc
                          [("format", (`String "date-time"));
                          ("description", (`String "A date-time string"));
                          ("type", (`String "string"))]]));
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
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
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
      let ppx_eds = ref [] in
      let ppx_body_grade =
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
        [("$defs", (`Assoc ([("grade", ppx_body_grade)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/grade"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type self_ref = {
  children: self_ref list }[@@deriving jsonschema]
include
  struct
    let self_ref_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_self_ref =
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
        [("$defs", (`Assoc ([("self_ref", ppx_body_self_ref)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/self_ref"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type two_self_refs = {
  a: self_ref ;
  b: self_ref }[@@deriving jsonschema]
include
  struct
    let two_self_refs_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("b",
                  ((match self_ref_jsonschema with
                    | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                        `Assoc
                          (("$id", (`String "file://shared/cases.ml:380")) ::
                          (List.filter
                             (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                             pairs))
                    | other -> other)));
               ("a",
                 ((match self_ref_jsonschema with
                   | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                       `Assoc
                         (("$id", (`String "file://shared/cases.ml:379")) ::
                         (List.filter
                            (fun (k, _) -> not (Stdlib.String.equal k "$id"))
                            pairs))
                   | other -> other)))]));
          ("required", (`List [`String "b"; `String "a"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('atom, 'group_atom) filter =
  | Atom of 'atom 
  | Group of ('atom, 'group_atom) filter list * 'group_atom [@@deriving
                                                              jsonschema]
include
  struct
    let filter_jsonschema atom group_atom =
      let ppx_eds = ref [] in
      let ppx_body_filter =
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
        [("$defs", (`Assoc ([("filter", ppx_body_filter)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/filter"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('atom, 'group_atom) bool_filter =
  | BoolAtom of ('atom, 'group_atom) filter 
  | BoolFilterGroup of ('atom, 'group_atom) bool_filter list [@@deriving
                                                               jsonschema]
include
  struct
    let bool_filter_jsonschema atom group_atom =
      let ppx_eds = ref [] in
      let ppx_body_bool_filter =
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
                                  (`String "file://shared/cases.ml:388"))
                               ::
                               (List.filter
                                  (fun (k, _) ->
                                     not (Stdlib.String.equal k "$id")) pairs))
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
        [("$defs",
           (`Assoc ([("bool_filter", ppx_body_bool_filter)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/bool_filter"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'a rec_wrapper =
  | RWrap of 'a 
  | RNested of 'a rec_wrapper [@@deriving jsonschema]
include
  struct
    let rec_wrapper_jsonschema a =
      let ppx_eds = ref [] in
      let ppx_body_rec_wrapper =
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
        [("$defs",
           (`Assoc ([("rec_wrapper", ppx_body_rec_wrapper)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/rec_wrapper"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type outer_rec =
  | ORLeaf of int 
  | ORNode of outer_rec rec_wrapper [@@deriving jsonschema]
include
  struct
    let outer_rec_jsonschema =
      let ppx_eds = ref [] in
      let ppx_body_outer_rec =
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
                                 (ppx_eds :=
                                    ((!ppx_eds) @
                                       (List.filter
                                          (fun (n, _) ->
                                             not
                                               (List.mem_assoc n (!ppx_eds)))
                                          ppx_defs));
                                  `Assoc
                                    (List.filter
                                       (fun (k, _) ->
                                          not (Stdlib.String.equal k "$defs"))
                                       ppx_pairs))
                             | _ -> `Assoc ppx_pairs)
                        | ppx_other -> ppx_other)]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      `Assoc
        [("$defs",
           (`Assoc ([("outer_rec", ppx_body_outer_rec)] @ (!ppx_eds))));
        ("$ref", (`String "#/$defs/outer_rec"))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_maximum = int[@@jsonschema.maximum 100][@@deriving jsonschema]
include
  struct
    let with_maximum_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc [("maximum", (`Int 100)); ("type", (`String "integer"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type with_maximum_record = {
  field: int [@jsonschema.maximum 100]}[@@deriving jsonschema]
include
  struct
    let with_maximum_record_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("field",
                  (`Assoc
                     [("maximum", (`Int 100)); ("type", (`String "integer"))]))]));
          ("required", (`List [`String "field"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type attrs_core_type =
  ((int)[@jsonschema.attrs
          {
            maximum = 100;
            minimum = 0;
            description = "Integer percentage value (0-100 inclusive)"
          }])[@@deriving jsonschema]
include
  struct
    let attrs_core_type_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("description",
             (`String "Integer percentage value (0-100 inclusive)"));
          ("minimum", (`Int 0));
          ("maximum", (`Int 100));
          ("type", (`String "integer"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type attrs_record =
  {
  score: int
    [@jsonschema.attrs
      { maximum = 100; minimum = 0; description = "Score out of 100" }];
  label: string
    [@jsonschema.attrs
      { format = "date-time"; description = "An ISO date-time" }]}[@@deriving
                                                                    jsonschema]
include
  struct
    let attrs_record_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("label",
                  (`Assoc
                     [("description", (`String "An ISO date-time"));
                     ("format", (`String "date-time"));
                     ("type", (`String "string"))]));
               ("score",
                 (`Assoc
                    [("description", (`String "Score out of 100"));
                    ("minimum", (`Int 0));
                    ("maximum", (`Int 100));
                    ("type", (`String "integer"))]))]));
          ("required", (`List [`String "label"; `String "score"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type attrs_type_decl = int[@@jsonschema.attrs
                            { description = "A plain integer" }][@@deriving
                                                                  jsonschema]
include
  struct
    let attrs_type_decl_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("description", (`String "A plain integer"));
          ("type", (`String "integer"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type minimum_core_type_int =
  ((int)[@jsonschema.minimum 0][@jsonschema.maximum 100])[@@deriving
                                                           jsonschema]
include
  struct
    let minimum_core_type_int_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("minimum", (`Int 0));
          ("maximum", (`Int 100));
          ("type", (`String "integer"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type minimum_core_type_float =
  ((float)[@jsonschema.minimum 0.0][@jsonschema.maximum 1.0])[@@deriving
                                                               jsonschema]
include
  struct
    let minimum_core_type_float_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("minimum", (`Float 0.0));
          ("maximum", (`Float 1.0));
          ("type", (`String "number"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type minimum_maximum_record =
  {
  score: int [@jsonschema.minimum 0][@jsonschema.maximum 100];
  ratio: float [@jsonschema.minimum 0.0][@jsonschema.maximum 1.0]}[@@deriving
                                                                    jsonschema]
include
  struct
    let minimum_maximum_record_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("ratio",
                  (`Assoc
                     [("minimum", (`Float 0.0));
                     ("maximum", (`Float 1.0));
                     ("type", (`String "number"))]));
               ("score",
                 (`Assoc
                    [("minimum", (`Int 0));
                    ("maximum", (`Int 100));
                    ("type", (`String "integer"))]))]));
          ("required", (`List [`String "ratio"; `String "score"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type minimum_maximum_type_decl_int = int[@@jsonschema.minimum 0][@@jsonschema.maximum
                                                                  255]
[@@deriving jsonschema]
include
  struct
    let minimum_maximum_type_decl_int_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("minimum", (`Int 0));
          ("maximum", (`Int 255));
          ("type", (`String "integer"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type minimum_maximum_type_decl_float = float[@@jsonschema.minimum 0.0]
[@@jsonschema.maximum 1.0][@@deriving jsonschema]
include
  struct
    let minimum_maximum_type_decl_float_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("minimum", (`Float 0.0));
          ("maximum", (`Float 1.0));
          ("type", (`String "number"))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type minimum_maximum_variant =
  | Percentage of ((int)[@jsonschema.minimum 0][@jsonschema.maximum 100]) 
  | Factor of ((float)[@jsonschema.minimum 0.0][@jsonschema.maximum 1.0]) 
[@@deriving jsonschema]
include
  struct
    let minimum_maximum_variant_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc
                   [("type", (`String "array"));
                   ("prefixItems",
                     (`List
                        [`Assoc [("const", (`String "Percentage"))];
                        `Assoc
                          [("minimum", (`Int 0));
                          ("maximum", (`Int 100));
                          ("type", (`String "integer"))]]));
                   ("unevaluatedItems", (`Bool false));
                   ("minItems", (`Int 2));
                   ("maxItems", (`Int 2))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "Factor"))];
                       `Assoc
                         [("minimum", (`Float 0.0));
                         ("maximum", (`Float 1.0));
                         ("type", (`String "number"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type variant_for_default =
  | A 
  | B [@@deriving (to_json, jsonschema)]
include
  struct
    [@@@ocaml.warning "-39-11-27"]
    let rec variant_for_default_to_json =
      (fun x ->
         match x with
         | A -> (Obj.magic [|(Obj.magic "A" : Js.Json.t)|] : Js.Json.t)
         | B -> (Obj.magic [|(Obj.magic "B" : Js.Json.t)|] : Js.Json.t) : 
      variant_for_default -> Js.Json.t)
    let variant_for_default_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
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
                  ("maxItems", (`Int 1))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type record_for_default = {
  score: int option }[@@deriving (to_json, jsonschema)]
include
  struct
    [@@@ocaml.warning "-39-11-27"]
    let rec record_for_default_to_json =
      (fun x ->
         match x with
         | { score = x_score } ->
             (Obj.magic
                (let module J =
                   struct
                     external unsafe_expr :
                       score:'a0 -> < score: 'a0   >  Js.t = "" ""[@@ocaml.warning
                                                                    "-unboxable-type-in-prim-decl"]
                     [@@mel.internal.ffi
                       "\132\149\166\190\000\000\000\012\000\000\000\005\000\000\000\r\000\000\000\012\145\160\160A\144%score@"]
                   end in
                   J.unsafe_expr
                     ~score:((option_to_json int_to_json) x_score)) : 
             Js.Json.t) : record_for_default -> Js.Json.t)
    let record_for_default_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("score",
                  (`Assoc
                     [("type", (`List [`String "integer"; `String "null"]))]))]));
          ("required", (`List [`String "score"]));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type default_value =
  {
  score: int option [@default 0];
  label: string [@jsonschema.default "default"];
  speed: float [@jsonschema.default 100.0];
  is_active: bool [@jsonschema.default false];
  pair: (int * string) [@jsonschema.default (1, "hello")];
  pairs: (string * string option) list
    [@default [("a", None); ("b", (Some "b"))]];
  variant: variant_for_default [@jsonschema.default A];
  record: record_for_default [@jsonschema.default { score = None }];
  int_list: int list [@jsonschema.default [1; 2; 3]];
  empty_list: int list [@jsonschema.default []]}[@@deriving jsonschema]
include
  struct
    let default_value_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("empty_list",
                  (`Assoc
                     [("default", (`List []));
                     ("type", (`String "array"));
                     ("items", (`Assoc [("type", (`String "integer"))]))]));
               ("int_list",
                 (`Assoc
                    [("default",
                       (((fun xs -> `List (List.map (fun x -> `Int x) xs)))
                          [1; 2; 3]));
                    ("type", (`String "array"));
                    ("items", (`Assoc [("type", (`String "integer"))]))]));
               ("record",
                 ((match match record_for_default_jsonschema with
                         | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                             `Assoc
                               (("$id",
                                  (`String "file://shared/cases.ml:438"))
                               ::
                               (List.filter
                                  (fun (k, _) ->
                                     not (Stdlib.String.equal k "$id")) pairs))
                         | other -> other
                   with
                   | `Assoc ppx_fields ->
                       `Assoc
                         (("default",
                            ((Ppx_deriving_jsonschema_runtime.classify
                                record_for_default_to_json) { score = None }))
                         :: ppx_fields)
                   | ppx_other -> ppx_other)));
               ("variant",
                 ((match match variant_for_default_jsonschema with
                         | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                             `Assoc
                               (("$id",
                                  (`String "file://shared/cases.ml:437"))
                               ::
                               (List.filter
                                  (fun (k, _) ->
                                     not (Stdlib.String.equal k "$id")) pairs))
                         | other -> other
                   with
                   | `Assoc ppx_fields ->
                       `Assoc
                         (("default",
                            ((Ppx_deriving_jsonschema_runtime.classify
                                variant_for_default_to_json) A))
                         :: ppx_fields)
                   | ppx_other -> ppx_other)));
               ("pairs",
                 (`Assoc
                    [("default",
                       (((fun xs ->
                            `List
                              (List.map
                                 (fun (ppx_tuple_0, ppx_tuple_1) ->
                                    `List
                                      [((fun x -> `String x)) ppx_tuple_0;
                                      ((fun x ->
                                          match x with
                                          | None -> `Null
                                          | Some v ->
                                              ((fun x -> `String x)) v))
                                        ppx_tuple_1]) xs)))
                          [("a", None); ("b", (Some "b"))]));
                    ("type", (`String "array"));
                    ("items",
                      (`Assoc
                         [("type", (`String "array"));
                         ("prefixItems",
                           (`List
                              [`Assoc [("type", (`String "string"))];
                              `Assoc
                                [("type",
                                   (`List [`String "string"; `String "null"]))]]));
                         ("unevaluatedItems", (`Bool false));
                         ("minItems", (`Int 2));
                         ("maxItems", (`Int 2))]))]));
               ("pair",
                 (`Assoc
                    [("default",
                       (((fun (ppx_tuple_0, ppx_tuple_1) ->
                            `List
                              [((fun x -> `Int x)) ppx_tuple_0;
                              ((fun x -> `String x)) ppx_tuple_1]))
                          (1, "hello")));
                    ("type", (`String "array"));
                    ("prefixItems",
                      (`List
                         [`Assoc [("type", (`String "integer"))];
                         `Assoc [("type", (`String "string"))]]));
                    ("unevaluatedItems", (`Bool false));
                    ("minItems", (`Int 2));
                    ("maxItems", (`Int 2))]));
               ("is_active",
                 (`Assoc
                    [("default", (((fun x -> `Bool x)) false));
                    ("type", (`String "boolean"))]));
               ("speed",
                 (`Assoc
                    [("default", (((fun x -> `Float x)) 100.0));
                    ("type", (`String "number"))]));
               ("label",
                 (`Assoc
                    [("default", (((fun x -> `String x)) "default"));
                    ("type", (`String "string"))]));
               ("score",
                 (`Assoc
                    [("default", (((fun x -> `Int x)) 0));
                    ("type", (`List [`String "integer"; `String "null"]))]))]));
          ("required", (`List []));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
module Status =
  struct
    type t =
      | Active 
      | Inactive [@@deriving (to_json, jsonschema)]
    include
      struct
        [@@@ocaml.warning "-39-11-27"]
        let rec to_json =
          (fun x ->
             match x with
             | Active ->
                 (Obj.magic [|(Obj.magic "Active" : Js.Json.t)|] : Js.Json.t)
             | Inactive ->
                 (Obj.magic [|(Obj.magic "Inactive" : Js.Json.t)|] : 
                 Js.Json.t) : t -> Js.Json.t)
        let t_jsonschema =
          let ppx_eds = ref [] in
          let ppx_result =
            `Assoc
              [("anyOf",
                 (`List
                    [`Assoc
                       [("type", (`String "array"));
                       ("prefixItems",
                         (`List [`Assoc [("const", (`String "Active"))]]));
                       ("unevaluatedItems", (`Bool false));
                       ("minItems", (`Int 1));
                       ("maxItems", (`Int 1))];
                    `Assoc
                      [("type", (`String "array"));
                      ("prefixItems",
                        (`List [`Assoc [("const", (`String "Inactive"))]]));
                      ("unevaluatedItems", (`Bool false));
                      ("minItems", (`Int 1));
                      ("maxItems", (`Int 1))]]))] in
          match !ppx_eds with
          | [] -> ppx_result
          | ppx_defs ->
              (match ppx_result with
               | `Assoc ppx_pairs ->
                   `Assoc (("$defs", (`Assoc ppx_defs)) ::
                     (List.filter
                        (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                        ppx_pairs))
               | other -> other)[@@warning "-32-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
type default_with_module_type =
  {
  status: Status.t [@jsonschema.default Status.Active]}[@@deriving
                                                         jsonschema]
include
  struct
    let default_with_module_type_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("status",
                  ((match match Status.t_jsonschema with
                          | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                              `Assoc
                                (("$id",
                                   (`String "file://shared/cases.ml:449"))
                                ::
                                (List.filter
                                   (fun (k, _) ->
                                      not (Stdlib.String.equal k "$id"))
                                   pairs))
                          | other -> other
                    with
                    | `Assoc ppx_fields ->
                        `Assoc
                          (("default",
                             ((Ppx_deriving_jsonschema_runtime.classify
                                 Status.to_json) Status.Active))
                          :: ppx_fields)
                    | ppx_other -> ppx_other)))]));
          ("required", (`List []));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type inner_with_option_field = {
  foo: int option [@option ][@drop_default ]}[@@deriving
                                               (to_json, jsonschema)]
include
  struct
    [@@@ocaml.warning "-39-11-27"]
    let rec inner_with_option_field_to_json =
      (fun x ->
         match x with
         | { foo = x_foo } ->
             (Obj.magic
                (let module J =
                   struct
                     external unsafe_expr :
                       foo:'a0 -> < foo: 'a0   >  Js.t = "" ""[@@ocaml.warning
                                                                "-unboxable-type-in-prim-decl"]
                     [@@mel.internal.ffi
                       "\132\149\166\190\000\000\000\n\000\000\000\005\000\000\000\012\000\000\000\012\145\160\160A\144#foo@"]
                   end in
                   J.unsafe_expr
                     ~foo:(match x_foo with
                           | Stdlib.Option.None -> Js.Undefined.empty
                           | Stdlib.Option.Some _ ->
                               Js.Undefined.return
                                 ((option_to_json int_to_json) x_foo))) : 
             Js.Json.t) : inner_with_option_field -> Js.Json.t)
    let inner_with_option_field_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("foo",
                  (`Assoc
                     [("type", (`List [`String "integer"; `String "null"]))]))]));
          ("required", (`List []));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let empty_inner_with_option_field : inner_with_option_field = { foo = None }
type outer_default_record_with_option =
  {
  inner: inner_with_option_field [@default empty_inner_with_option_field]}
[@@deriving jsonschema]
include
  struct
    let outer_default_record_with_option_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("type", (`String "object"));
          ("properties",
            (`Assoc
               [("inner",
                  ((match match inner_with_option_field_jsonschema with
                          | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                              `Assoc
                                (("$id",
                                   (`String "file://shared/cases.ml:458"))
                                ::
                                (List.filter
                                   (fun (k, _) ->
                                      not (Stdlib.String.equal k "$id"))
                                   pairs))
                          | other -> other
                    with
                    | `Assoc ppx_fields ->
                        `Assoc
                          (("default",
                             ((Ppx_deriving_jsonschema_runtime.classify
                                 inner_with_option_field_to_json)
                                empty_inner_with_option_field))
                          :: ppx_fields)
                    | ppx_other -> ppx_other)))]));
          ("required", (`List []));
          ("additionalProperties", (`Bool false))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type compact_variants =
  | A 
  | B 
  | C of int [@@deriving jsonschema][@@jsonschema.compact_variants ]
include
  struct
    let compact_variants_jsonschema =
      let ppx_eds = ref [] in
      let ppx_result =
        `Assoc
          [("anyOf",
             (`List
                [`Assoc [("const", (`String "A"))];
                `Assoc [("const", (`String "B"))];
                `Assoc
                  [("type", (`String "array"));
                  ("prefixItems",
                    (`List
                       [`Assoc [("const", (`String "C"))];
                       `Assoc [("type", (`String "integer"))]]));
                  ("unevaluatedItems", (`Bool false));
                  ("minItems", (`Int 2));
                  ("maxItems", (`Int 2))]]))] in
      match !ppx_eds with
      | [] -> ppx_result
      | ppx_defs ->
          (match ppx_result with
           | `Assoc ppx_pairs ->
               `Assoc (("$defs", (`Assoc ppx_defs)) ::
                 (List.filter
                    (fun (k, _) -> not (Stdlib.String.equal k "$defs"))
                    ppx_pairs))
           | other -> other)[@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
