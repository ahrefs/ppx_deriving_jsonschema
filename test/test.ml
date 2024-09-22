[@@@ocaml.warning "-37-69"]

let print_schema ?definitions ?id ?title ?description s =
  let s = Ppx_deriving_jsonschema_runtime.json_schema ?definitions ?id ?title ?description s in
  let () = print_endline (Yojson.Basic.pretty_to_string s) in
  ()

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

let () = print_schema with_modules_jsonschema

type kind =
  | Success
  | Error
  | Skipped [@name "skipped"]
[@@deriving jsonschema]

let () = print_schema kind_jsonschema

type poly_kind =
  [ `Aaa
  | `Bbb
  | `Ccc [@name "ccc"]
  ]
[@@deriving jsonschema]

let () = print_schema poly_kind_jsonschema

type poly_inherit =
  [ `New_one
  | poly_kind
  ]
[@@deriving jsonschema]

let () = print_schema poly_inherit_jsonschema

type event = {
  date : float;
  kind_f : kind;
  comment : string;
  opt : int option; [@key "opt_int"]
  a : float array;
  l : string list;
  t : [ `Foo | `Bar | `Baz ];
}
[@@deriving jsonschema]

let () = print_schema event_jsonschema

(* type recursive_record = {
     a : int;
     b : recursive_record list;
   }
   [@@deriving jsonschema]

   let () = print_schema recursive_record_jsonschema

   type recursive_variant =
     | A of recursive_variant
     | B
   [@@deriving jsonschema]

   let () = print_schema recursive_variant_jsonschema *)

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

type using_m = { m : Mod1.m_1 } [@@deriving jsonschema]

let () = print_schema using_m_jsonschema

type 'param poly = { f : 'param } [@@deriving jsonschema]

let () = print_schema poly_jsonschema

type 'param2 poly2 = C of 'param2 [@@deriving jsonschema]

let () = print_schema poly2_jsonschema

type tuple_with_variant = int * [ `A | `B [@name "second_cstr"] ] [@@deriving jsonschema]

let () = print_schema tuple_with_variant_jsonschema

type player_scores = {
  player : string;
  scores : numbers; [@ref "numbers"] [@key "scores_ref"]
}
[@@deriving jsonschema]

let () =
  print_schema ~id:"https://ahrefs.com/schemas/player_scores" ~title:"Player scores"
    ~description:"Object representing player scores"
    ~definitions:[ "numbers", numbers_jsonschema ]
    player_scores_jsonschema
