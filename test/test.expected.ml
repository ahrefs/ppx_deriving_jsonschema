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
             [("date", (`Assoc [("type", (`String "float"))]));
             ("kind_f", (`Assoc [("$ref", (`String "#/definitions/kind"))]));
             ("comment", (`Assoc [("type", (`String "string"))]));
             ("t", (`Assoc []))]))][@@warning "-32"]
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
        ("items", (`Assoc [("type", (`String "int"))]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
