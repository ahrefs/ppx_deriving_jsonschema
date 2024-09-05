[@@@ocaml.warning "-37-69"]

type kind =
  | Success
  | Error
  | Skipped
[@@deriving jsonschema]

type event = {
  date : float;
  kind : kind;
  comment : string;
  t : [ `Foo | `Bar | `Baz ];
}
[@@deriving jsonschema]
