[@@@ocaml.warning "-37-69"]
let print_schema s =
  let s = Ppx_deriving_jsonschema_runtime.json_schema s in
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
            [("type", (`String "string"));
            ("enum", (`List [`String "A"; `String "B"]))][@@warning "-32-39"]
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
                [("type", (`String "string"));
                ("enum", (`List [`String "C"; `String "D"]))][@@warning
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
        [("type", (`String "string"));
        ("enum",
          (`List [`String "Success"; `String "Error"; `String "skipped"]))]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema kind_jsonschema
type poly_kind = [ `Aaa  | `Bbb  | `Ccc [@name "ccc"]][@@deriving jsonschema]
include
  struct
    let poly_kind_jsonschema =
      `Assoc
        [("type", (`String "string"));
        ("enum", (`List [`String "Aaa"; `String "Bbb"; `String "ccc"]))]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly_kind_jsonschema
type poly_inherit = [ `New_one  | poly_kind][@@deriving jsonschema]
include
  struct
    let poly_inherit_jsonschema =
      `Assoc
        [("type", (`String "string"));
        ("enum",
          (`List
             [`String "New_one";
             `String
               "unsupported polymorphic variant type: [ `New_one  | poly_kind]"]))]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly_inherit_jsonschema
type event =
  {
  date: float ;
  kind_f: kind ;
  comment: string ;
  opt: int option [@key "opt_int"];
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
             [("t",
                (`Assoc
                   [("type", (`String "string"));
                   ("enum",
                     (`List [`String "Foo"; `String "Bar"; `String "Baz"]))]));
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
             [`String "t";
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
        ("items",
          (`List [event_jsonschema; `Assoc [("type", (`String "string"))]]))]
      [@@warning "-32-39"]
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
             ("items",
               (`List
                  [event_jsonschema; `Assoc [("type", (`String "integer"))]]))]))]
      [@@warning "-32-39"]
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
      `Assoc
        [("type", (`String "string"));
        ("enum",
          (`List
             [`String
                "unsuported variant constructor with a payload: \n| C of 'param2\n"]))]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema poly2_jsonschema
type tuple_with_variant = (int * [ `A  | `B [@name "second_cstr"]])[@@deriving
                                                                    jsonschema]
include
  struct
    let tuple_with_variant_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          (`List
             [`Assoc [("type", (`String "integer"))];
             `Assoc
               [("type", (`String "string"));
               ("enum", (`List [`String "A"; `String "second_cstr"]))]]))]
      [@@warning "-32-39"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let () = print_schema tuple_with_variant_jsonschema
