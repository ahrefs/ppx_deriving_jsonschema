[@@@ocaml.warning "-37-69"]

type kind =
  | Success
  | Error
  | Skipped
[@@deriving jsonschema]

type event = {
  date : float;
  kind_f : kind;
  comment : string;
  t : [ `Foo | `Bar | `Baz ];
}
[@@deriving jsonschema]

type events = event list [@@deriving jsonschema]

type eventss = event list list [@@deriving jsonschema]

type event_comment = event * string [@@deriving jsonschema]

type event_comments = event * string list [@@deriving jsonschema]

type event_comments' = event_comment list [@@deriving jsonschema]

type event_n = (event * int) list [@@deriving jsonschema]

type events_array = events array [@@deriving jsonschema]

type numbers = int list [@@deriving jsonschema]
