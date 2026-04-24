open Melange_json.Primitives

type variant_for_default =
  | A
  | B
[@@deriving to_json, jsonschema]

type record_for_default = { score : int option } [@@deriving to_json, jsonschema]

type default_value = {
  score : int option; [@default 0]
  label : string; [@jsonschema.default "default"]
  speed : float; [@jsonschema.default 100.0]
  is_active : bool; [@jsonschema.default false]
  pair : int * string; [@jsonschema.default 1, "hello"]
  pairs : (string * string option) list; [@default [ "a", None; "b", Some "b" ]]
  variant : variant_for_default; [@jsonschema.default A]
  record : record_for_default; [@jsonschema.default { score = None }]
  int_list : int list; [@jsonschema.default [ 1; 2; 3 ]]
  empty_list : int list; [@jsonschema.default []]
}
[@@deriving jsonschema]

module Status = struct
  type t =
    | Active
    | Inactive
  [@@deriving to_json, jsonschema]
end

type default_with_module_type = { status : Status.t [@jsonschema.default Status.Active] } [@@deriving jsonschema]

let default_snapshot =
  `Assoc
    [
      "$schema", `String Ppx_deriving_jsonschema_runtime.schema_version;
      ( "oneOf",
        `List
          [
            Ppx_deriving_jsonschema_runtime.json_schema default_value_jsonschema;
            Ppx_deriving_jsonschema_runtime.json_schema default_with_module_type_jsonschema;
          ] );
    ]

let default_snapshot_string = Schema_snapshot.json_to_string default_snapshot

let string_jsonschema = `Assoc [ "type", `String "string" ]
let int_jsonschema = `Assoc [ "type", `String "integer" ]
let bool_jsonschema = `Assoc [ "type", `String "boolean" ]

module Mod1 = struct
  type m_1 =
    | A
    | B
  [@@deriving jsonschema]

  module Mod2 = struct
    type m_2 =
      | C
      | D
    [@@deriving jsonschema]
  end
end

type with_modules = {
  m : Mod1.m_1;
  m2 : Mod1.Mod2.m_2;
}
[@@deriving jsonschema]

type kind =
  | Success
  | Error
  | Skipped [@name "skipped"]
[@@deriving jsonschema]

type kind_as_string =
  | Success
  | Error
  | Skipped [@name "skipped"]
[@@deriving jsonschema ~variant_as_string]

type poly_kind =
  [ `Aaa
  | `Bbb
  | `Ccc [@name "ccc"]
  ]
[@@deriving jsonschema]

type poly_kind_as_string =
  [ `Aaa
  | `Bbb
  | `Ccc [@name "ccc"]
  ]
[@@deriving jsonschema ~variant_as_string]

type poly_kind_with_payload =
  [ `Aaa of int
  | `Bbb
  | `Ccc of string * bool [@name "ccc"]
  ]
[@@deriving jsonschema]

type poly_kind_with_payload_as_string =
  [ `Aaa of int
  | `Bbb
  | `Ccc of string * bool [@name "ccc"]
  ]
[@@deriving jsonschema ~variant_as_string]

type poly_inherit =
  [ `New_one
  | `Second_one of int
  | poly_kind
  ]
[@@deriving jsonschema]

type poly_inherit_as_string =
  [ `New_one
  | `Second_one of int
  | poly_kind_as_string
  ]
[@@deriving jsonschema ~variant_as_string]

type event = {
  date : float;
  kind_f : kind;
  comment : string;
  opt : int option; [@key "opt_int"]
  a : float array;
  l : string list;
  t : [ `Foo | `Bar | `Baz ];
  c : char;
  bunch_of_bytes : bytes;
  string_ref : string ref;
  unit : unit;
  native_int : nativeint;
}
[@@deriving jsonschema]

type events = event list [@@deriving jsonschema]
type eventss = event list list [@@deriving jsonschema]
type event_comment = event * string [@@deriving jsonschema]
type event_comments' = event_comment list [@@deriving jsonschema]
type event_n = (event * int) list [@@deriving jsonschema]
type events_array = events array [@@deriving jsonschema]
type numbers = int list [@@deriving jsonschema]
type opt = int option [@@deriving jsonschema]
type using_m = { m : Mod1.m_1 } [@@deriving jsonschema]

type 'param2 poly2 = C of 'param2 [@@deriving jsonschema ~variant_as_string]

type tuple_with_variant = int * [ `A | `B [@name "second_cstr"] ] [@@deriving jsonschema]

type player_scores = {
  player : string;
  scores : numbers; [@ref "numbers"] [@key "scores_ref"]
}
[@@deriving jsonschema]

type address = {
  street : string;
  city : string;
  zip : string;
}
[@@deriving jsonschema]

type t = {
  name : string;
  age : int;
  email : string option;
  address : address;
}
[@@deriving jsonschema]

type tt = {
  name : string;
  age : int;
  email : string option;
  home_address : address; [@ref "shared_address"]
  work_address : address; [@ref "shared_address"]
  retreat_address : address; [@ref "shared_address"]
}
[@@deriving jsonschema]

type c = char [@@deriving jsonschema]

type variant_inline_record =
  | A of { a : int }
  | B of { b : string }
[@@deriving jsonschema ~variant_as_string]

type variant_with_payload =
  | A of int
  | B
  | C of int * string
  | D of (int * string * bool)
[@@deriving jsonschema ~variant_as_string]

let schemas =
  [
    Ppx_deriving_jsonschema_runtime.json_schema with_modules_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema kind_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema kind_as_string_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_kind_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_kind_as_string_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_kind_with_payload_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_kind_with_payload_as_string_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_inherit_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_inherit_as_string_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema event_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema events_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema eventss_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema event_comment_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema event_comments'_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema event_n_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema events_array_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema numbers_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema opt_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema using_m_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema (poly2_jsonschema int_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema tuple_with_variant_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema ~id:"https://ahrefs.com/schemas/player_scores" ~title:"Player scores"
      ~description:"Object representing player scores"
      ~definitions:[ "numbers", numbers_jsonschema ]
      player_scores_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema ~definitions:[ "shared_address", address_jsonschema ] tt_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema c_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema variant_inline_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema variant_with_payload_jsonschema;
  ]

let snapshot = `Assoc [ "$schema", `String Ppx_deriving_jsonschema_runtime.schema_version; "oneOf", `List schemas ]

let snapshot_string = Schema_snapshot.json_to_string snapshot
