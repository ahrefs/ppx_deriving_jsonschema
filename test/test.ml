[@@@ocaml.warning "-37-69"]

let print_schema s =
  let s = Ppx_deriving_jsonschema_runtime.json_schema s in
  let () = print_endline (Yojson.Basic.pretty_to_string s) in
  ()

module M = struct
  type m_1 =
    | A
    | B
  [@@deriving jsonschema]
end

type kind =
  | Success
  | Error
  | Skipped
[@@deriving jsonschema]

let () = print_schema kind_jsonschema

type event = {
  date : float;
  kind_f : kind;
  comment : string;
  opt : int option;
  a : float array;
  l : string list;
  t : [ `Foo | `Bar | `Baz ];
}
[@@deriving jsonschema]

let () = print_schema event_jsonschema

type recursive_record = {
  a : int;
  b : recursive_record list;
}
[@@deriving jsonschema]

let () = print_schema recursive_record_jsonschema

type recursive_variant =
  | A of recursive_variant
  | B
[@@deriving jsonschema]

let () = print_schema recursive_variant_jsonschema

type events = event list [@@deriving jsonschema]

let () = print_schema events_jsonschema

type eventss = event list list [@@deriving jsonschema]

let () = print_schema eventss_jsonschema

type event_comment = event * string [@@deriving jsonschema]

let () = print_schema event_comment_jsonschema

type event_comments' = event_comment list [@@deriving jsonschema]

let () = print_schema event_comments'_jsonschema

type event_n = (event * int) list [@@deriving jsonschema]

let () = print_schema event_n_jsonschema

type events_array = events array [@@deriving jsonschema]

let () = print_schema events_array_jsonschema

type numbers = int list [@@deriving jsonschema]

let () = print_schema numbers_jsonschema

type opt = int option [@@deriving jsonschema]

let () = print_schema opt_jsonschema

type using_m = { m : M.m_1 } [@@deriving jsonschema]

let () = print_schema using_m_jsonschema

type 'param poly = { f : 'param } [@@deriving jsonschema]

let () = print_schema poly_jsonschema

type 'param2 poly2 = C of 'param2 [@@deriving jsonschema]

let () = print_schema poly2_jsonschema
