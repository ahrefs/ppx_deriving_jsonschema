[@@@ocaml.warning "-37-69"]
let print_schema s =
  let s = Ppx_deriving_jsonschema_runtime.json_schema s in
  let () = print_endline (Yojson.Basic.pretty_to_string s) in ()
module M =
  struct
    type m_1 =
      | A 
      | B [@@deriving jsonschema]
    include
      struct
        let m_1_jsonschema =
          `Assoc
            [("type", (`String "string"));
            ("enum", (`List [`String "A"; `String "B"]))][@@warning "-32"]
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
type kind =
  | Success 
  | Error 
  | Skipped [@@deriving jsonschema]
include
  struct
    let kind_jsonschema =
      `Assoc
        [("type", (`String "string"));
        ("enum",
          (`List [`String "Success"; `String "Error"; `String "Skipped"]))]
      [@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema kind_jsonschema
type event =
  {
  date: float ;
  kind_f: kind ;
  comment: string ;
  opt: int option ;
  a: float array ;
  l: string list ;
  t: [ `Foo  | `Bar  | `Baz ] }[@@deriving jsonschema]
include
  struct
    let event_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("t", (`Assoc []));
             ("l",
               (`Assoc
                  [("type", (`String "array"));
                  ("items", (`Assoc [("type", (`String "string"))]))]));
             ("a",
               (`Assoc
                  [("type", (`String "array"));
                  ("items", (`Assoc [("type", (`String "number"))]))]));
             ("opt", (`Assoc [("type", (`String "integer"))]));
             ("comment", (`Assoc [("type", (`String "string"))]));
             ("kind_f", (`Assoc [("$ref", (`String "#/definitions/kind"))]));
             ("date", (`Assoc [("type", (`String "number"))]))]));
        ("required",
          (`List
             [`String "t";
             `String "l";
             `String "a";
             `String "comment";
             `String "kind_f";
             `String "date"]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema event_jsonschema
type recursive_record = {
  a: int ;
  b: recursive_record list }[@@deriving jsonschema]
include
  struct
    let recursive_record_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("b",
                (`Assoc
                   [("type", (`String "array"));
                   ("items",
                     (`Assoc
                        [("$ref", (`String "#/definitions/recursive_record"))]))]));
             ("a", (`Assoc [("type", (`String "integer"))]))]));
        ("required", (`List [`String "b"; `String "a"]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema recursive_record_jsonschema
type recursive_variant =
  | A of recursive_variant 
  | B [@@deriving jsonschema]
include
  struct
    let recursive_variant_jsonschema =
      `Assoc [("type", (`String "string")); ("enum", (`List [`String "B"]))]
      [@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema recursive_variant_jsonschema
type events = event list[@@deriving jsonschema]
include
  struct
    let events_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items", (`Assoc [("$ref", (`String "#/definitions/event"))]))]
      [@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema events_jsonschema
type eventss = event list list[@@deriving jsonschema]
include
  struct
    let eventss_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          (`Assoc
             [("type", (`String "array"));
             ("items", (`Assoc [("$ref", (`String "#/definitions/event"))]))]))]
      [@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema eventss_jsonschema
type event_comment = (event * string)[@@deriving jsonschema]
include
  struct
    let event_comment_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          (`List
             [`Assoc [("$ref", (`String "#/definitions/event"))];
             `Assoc [("type", (`String "string"))]]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema event_comment_jsonschema
type event_comments' = event_comment list[@@deriving jsonschema]
include
  struct
    let event_comments'_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          (`Assoc [("$ref", (`String "#/definitions/event_comment"))]))]
      [@@warning "-32"]
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
             ("items",
               (`List
                  [`Assoc [("$ref", (`String "#/definitions/event"))];
                  `Assoc [("type", (`String "integer"))]]))]))][@@warning
                                                                 "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema event_n_jsonschema
type events_array = events array[@@deriving jsonschema]
include
  struct
    let events_array_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items", (`Assoc [("$ref", (`String "#/definitions/events"))]))]
      [@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema events_array_jsonschema
type numbers = int list[@@deriving jsonschema]
include
  struct
    let numbers_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items", (`Assoc [("type", (`String "integer"))]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema numbers_jsonschema
type opt = int option[@@deriving jsonschema]
include
  struct
    let opt_jsonschema = `Assoc [("type", (`String "integer"))][@@warning
                                                                 "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema opt_jsonschema
type using_m = {
  m: M.m_1 }[@@deriving jsonschema]
include
  struct
    let using_m_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties", (`Assoc [("m", (`Assoc []))]));
        ("required", (`List [`String "m"]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema using_m_jsonschema
type 'param poly = {
  f: 'param }[@@deriving jsonschema]
include
  struct
    let poly_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties", (`Assoc [("f", (`Assoc []))]));
        ("required", (`List [`String "f"]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly_jsonschema
type 'param2 poly2 =
  | C of 'param2 [@@deriving jsonschema]
include
  struct
    let poly2_jsonschema =
      `Assoc [("type", (`String "string")); ("enum", (`List []))][@@warning
                                                                   "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly2_jsonschema
