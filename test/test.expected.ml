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
            [("oneOf",
               (`List
                  [`Assoc [("const", (`String "A"))];
                  `Assoc [("const", (`String "B"))]]))][@@warning "-32-39"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
    module Mod2 =
      struct
        type m_2 =
          | C 
          | D [@@deriving jsonschema]
        include
          struct
            let m_2_jsonschema =
              `Assoc
                [("oneOf",
                   (`List
                      [`Assoc [("const", (`String "C"))];
                      `Assoc [("const", (`String "D"))]]))][@@warning
                                                             "-32-39"]
          end[@@ocaml.doc "@inline"][@@merlin.hide ]
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
let () = print_schema with_modules_jsonschema
type kind =
  | Success 
  | Error 
  | Skipped [@name "skipped"][@@deriving jsonschema]
include
  struct
    let kind_jsonschema =
      `Assoc
        [("oneOf",
           (`List
              [`Assoc [("const", (`String "Success"))];
              `Assoc [("const", (`String "Error"))];
              `Assoc [("const", (`String "skipped"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema kind_jsonschema
type kind_as_array =
  | Success 
  | Error 
  | Skipped [@name "skipped"][@@deriving jsonschema ~variant_as_array]
include
  struct
    let kind_as_array_jsonschema =
      `Assoc
        [("oneOf",
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
let () = print_schema kind_as_array_jsonschema
type poly_kind = [ `Aaa  | `Bbb  | `Ccc [@name "ccc"]][@@deriving jsonschema]
include
  struct
    let poly_kind_jsonschema =
      `Assoc
        [("oneOf",
           (`List
              [`Assoc [("const", (`String "Aaa"))];
              `Assoc [("const", (`String "Bbb"))];
              `Assoc [("const", (`String "ccc"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly_kind_jsonschema
type poly_kind_as_array = [ `Aaa  | `Bbb  | `Ccc [@name "ccc"]][@@deriving
                                                                 jsonschema
                                                                   ~variant_as_array]
include
  struct
    let poly_kind_as_array_jsonschema =
      `Assoc
        [("oneOf",
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
let () = print_schema poly_kind_as_array_jsonschema
type poly_kind_with_payload =
  [ `Aaa of int  | `Bbb  | `Ccc of (string * bool) [@name "ccc"]][@@deriving
                                                                   jsonschema]
include
  struct
    let poly_kind_with_payload_jsonschema =
      `Assoc
        [("oneOf",
           (`List
              [`Assoc [("const", (`String "Aaa"))];
              `Assoc [("const", (`String "Bbb"))];
              `Assoc [("const", (`String "ccc"))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly_kind_with_payload_jsonschema
type poly_kind_with_payload_as_array =
  [ `Aaa of int  | `Bbb  | `Ccc of (string * bool) [@name "ccc"]][@@deriving
                                                                   jsonschema
                                                                    ~variant_as_array]
include
  struct
    let poly_kind_with_payload_as_array_jsonschema =
      `Assoc
        [("oneOf",
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
let () = print_schema poly_kind_with_payload_as_array_jsonschema
type poly_inherit = [ `New_one  | `Second_one of int  | poly_kind][@@deriving
                                                                    jsonschema]
include
  struct
    let poly_inherit_jsonschema =
      `Assoc
        [("oneOf",
           (`List
              [`Assoc [("const", (`String "New_one"))];
              `Assoc [("const", (`String "Second_one"))];
              poly_kind_jsonschema]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly_inherit_jsonschema
type poly_inherit_as_array =
  [ `New_one  | `Second_one of int  | poly_kind_as_array][@@deriving
                                                           jsonschema
                                                             ~variant_as_array]
include
  struct
    let poly_inherit_as_array_jsonschema =
      `Assoc
        [("oneOf",
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
let () = print_schema poly_inherit_as_array_jsonschema
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
                  [("oneOf",
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
let () = print_schema event_jsonschema
type events = event list[@@deriving jsonschema]
include
  struct
    let events_jsonschema =
      `Assoc [("type", (`String "array")); ("items", event_jsonschema)]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema events_jsonschema
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
let () = print_schema eventss_jsonschema
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
let () = print_schema event_comment_jsonschema
type event_comments' = event_comment list[@@deriving jsonschema]
include
  struct
    let event_comments'_jsonschema =
      `Assoc
        [("type", (`String "array")); ("items", event_comment_jsonschema)]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema event_comments'_jsonschema
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
let () = print_schema event_n_jsonschema
type events_array = events array[@@deriving jsonschema]
include
  struct
    let events_array_jsonschema =
      `Assoc [("type", (`String "array")); ("items", events_jsonschema)]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema events_array_jsonschema
type numbers = int list[@@deriving jsonschema]
include
  struct
    let numbers_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items", (`Assoc [("type", (`String "integer"))]))][@@warning
                                                              "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema numbers_jsonschema
type opt = int option[@@deriving jsonschema]
include
  struct
    let opt_jsonschema = `Assoc [("type", (`String "integer"))][@@warning
                                                                 "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema opt_jsonschema
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
let () = print_schema using_m_jsonschema
type 'param poly = {
  f: 'param }[@@deriving jsonschema]
include
  struct
    let poly_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("f", (`Assoc [("unsuported core type", (`String "'param"))]))]));
        ("required", (`List [`String "f"]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly_jsonschema
type 'param2 poly2 =
  | C of 'param2 [@@deriving jsonschema]
include
  struct
    let poly2_jsonschema =
      `Assoc [("oneOf", (`List [`Assoc [("const", (`String "C"))]]))]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly2_jsonschema
type 'param2 poly2_as_array =
  | C of 'param2 [@@deriving jsonschema ~variant_as_array]
include
  struct
    let poly2_as_array_jsonschema =
      `Assoc
        [("oneOf",
           (`List
              [`Assoc
                 [("type", (`String "array"));
                 ("prefixItems",
                   (`List
                      [`Assoc [("const", (`String "C"))];
                      `Assoc [("unsuported core type", (`String "'param2"))]]));
                 ("unevaluatedItems", (`Bool false));
                 ("minItems", (`Int 2));
                 ("maxItems", (`Int 2))]]))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly2_as_array_jsonschema
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
               [("oneOf",
                  (`List
                     [`Assoc [("const", (`String "A"))];
                     `Assoc [("const", (`String "second_cstr"))]]))]]));
        ("unevaluatedItems", (`Bool false));
        ("minItems", (`Int 2));
        ("maxItems", (`Int 2))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema tuple_with_variant_jsonschema
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
let () =
  print_schema ~id:"https://ahrefs.com/schemas/player_scores"
    ~title:"Player scores" ~description:"Object representing player scores"
    ~definitions:[("numbers", numbers_jsonschema)] player_scores_jsonschema
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
let () = print_schema t_jsonschema
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
let () =
  print_schema ~definitions:[("shared_address", address_jsonschema)]
    tt_jsonschema
type c = char[@@deriving jsonschema]
include
  struct
    let c_jsonschema =
      `Assoc
        [("type", (`String "string"));
        ("minLength", (`Int 1));
        ("maxLength", (`Int 1))][@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema c_jsonschema
type variant_inline_record =
  | A of {
  a: int } 
  | B of {
  b: string } [@@deriving jsonschema ~variant_as_array]
include
  struct
    let variant_inline_record_jsonschema =
      `Assoc
        [("oneOf",
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
let () = print_schema variant_inline_record_jsonschema
type variant_with_payload =
  | A of int 
  | B 
  | C of int * string 
  | D of (int * string * bool) [@@deriving jsonschema ~variant_as_array]
include
  struct
    let variant_with_payload_jsonschema =
      `Assoc
        [("oneOf",
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
let () = print_schema variant_with_payload_jsonschema
