[@@@ocaml.warning "-37-69"]
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
  t: [ `Foo  | `Bar  | `Baz ] }[@@deriving jsonschema]
include
  struct
    let event_jsonschema =
      `Assoc
        [("type", (`String "object"));
        ("properties",
          (`Assoc
             [("date", (`Assoc [("type", (`String "number"))]));
             ("kind_f", (`Assoc [("$ref", (`String "#/definitions/kind"))]));
             ("comment", (`Assoc [("type", (`String "string"))]));
             ("t", (`Assoc []))]));
        ("required", (`List []))][@@warning "-32"]
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
