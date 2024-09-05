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
  kind: kind ;
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
             ("kind", (`Assoc [("$ref", (`String "#/definitions/kind"))]));
             ("comment", (`Assoc [("type", (`String "string"))]));
             ("t", (`Assoc []))]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
