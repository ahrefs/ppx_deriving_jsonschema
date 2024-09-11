[@@@ocaml.warning "-37-69"]
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
type event =
  {
  date: float ;
  kind_f: kind ;
  comment: string ;
  opt: int option ;
  t: [ `Foo  | `Bar  | `Baz ] }[@@deriving jsonschema]
include
  struct
    let event_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("t", (`Assoc []));
             ("opt", (`Assoc [("type", (`String "integer"))]));
             ("comment", (`Assoc [("type", (`String "string"))]));
             ("kind_f", (`Assoc [("$ref", (`String "#/definitions/kind"))]));
             ("date", (`Assoc [("type", (`String "number"))]))]));
        ("required", (`List ["t"; "comment"; "kind_f"; "date"]))][@@warning
                                                                   "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type events = event list[@@deriving jsonschema]
include
  struct
    let events_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items", (`Assoc [("$ref", (`String "#/definitions/event"))]))]
      [@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
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
type event_comments = (event * string list)[@@deriving jsonschema]
include
  struct
    let event_comments_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items",
          (`List
             [`Assoc [("$ref", (`String "#/definitions/event"))];
             `Assoc
               [("type", (`String "array"));
               ("items", (`Assoc [("type", (`String "string"))]))]]))]
      [@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
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
type events_array = events array[@@deriving jsonschema]
include
  struct
    let events_array_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items", (`Assoc [("$ref", (`String "#/definitions/events"))]))]
      [@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type numbers = int list[@@deriving jsonschema]
include
  struct
    let numbers_jsonschema =
      `Assoc
        [("type", (`String "array"));
        ("items", (`Assoc [("type", (`String "integer"))]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type opt = int option[@@deriving jsonschema]
include
  struct
    let opt_jsonschema = `Assoc [("type", (`String "integer"))][@@warning
                                                                 "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type using_m = {
  m: M.m_1 }[@@deriving jsonschema]
include
  struct
    let using_m_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties", (`Assoc [("m", (`Assoc []))]));
        ("required", (`List ["m"]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'param poly = {
  f: 'param }[@@deriving jsonschema]
include
  struct
    let poly_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties", (`Assoc [("f", (`Assoc []))]));
        ("required", (`List ["f"]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'param2 poly2 =
  | C of 'param2 [@@deriving jsonschema]
include
  struct
    let poly2_jsonschema =
      `Assoc [("type", (`String "string")); ("enum", (`List []))][@@warning
                                                                   "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
