[@@@ocaml.warning "-37-69"]
open Melange_json.Primitives

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
type recursive_record = {
  a : int;
  b : recursive_record list;
}
[@@deriving jsonschema]
type recursive_variant =
  | A of recursive_variant
  | B
[@@deriving jsonschema]
type tree =
  | Leaf
  | Node of {
      value : int;
      left : tree;
      right : tree;
    }
[@@deriving jsonschema]
type non_recursive = {
  x : int;
  y : string;
}
[@@deriving jsonschema]
type foo = { bar : bar option }
and bar = { foo : foo option } [@@deriving jsonschema]
type expr =
  | Literal of int
  | Binary of expr * expr
  | Block of stmt list
and stmt =
  | ExprStmt of expr
  | IfStmt of {
      cond : expr;
      then_ : stmt;
      else_ : stmt option;
    }
[@@deriving jsonschema]
type alpha = { x : int }
and beta = { y : string } [@@deriving jsonschema]
type node_a = {
  b : node_b option;
  c : node_c option;
}
and node_b = {
  a : node_a option;
  c : node_c option;
}
and node_c = {
  a : node_a option;
  b : node_b option;
}
[@@deriving jsonschema]
type recursive_tuple =
  | Leaf of int
  | Branch of (recursive_tuple * recursive_tuple)
[@@deriving jsonschema]
type int_tree = tree [@@deriving jsonschema]
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
type inline_record_with_extra_fields =
  | User of {
      name : string;
      email : string;
    } [@jsonschema.allow_extra_fields]
  | Guest of { ip : string }
[@@deriving jsonschema]
type variant_with_payload =
  | A of int
  | B
  | C of int * string
  | D of (int * string * bool)
[@@deriving jsonschema ~variant_as_string]
type t1 =
  | Typ
  | Class of string
[@@deriving jsonschema]
type t2 =
  | Typ
  | Class of string
[@@deriving jsonschema ~variant_as_string]
type t3 =
  | Typ [@name "type"]
  | Class of string [@name "class"]
[@@deriving jsonschema]
type t4 = int * string [@@deriving jsonschema]
type t5 = [ `A of int * string * bool ] [@@deriving jsonschema]
type t6 = [ `A of (int * string * bool) * float ] [@@deriving jsonschema]
type t7 = A of int * string * bool [@@deriving jsonschema]
type t8 = A of (int * string * bool) [@@deriving jsonschema]
type t9 = A of (int * string * bool) * float [@@deriving jsonschema]
type t10 = [ `A of int * string * bool ] [@@deriving jsonschema]
type t11 = [ `B of int * string * bool ] [@@deriving jsonschema ~polymorphic_variant_tuple]
type obj2 = { x : int } [@@deriving jsonschema] [@@jsonschema.allow_extra_fields]
type obj1 = { obj2 : obj2 } [@@deriving jsonschema]
type nested_obj = { obj1 : obj1 } [@@deriving jsonschema] [@@allow_extra_fields]
type x_without_extra = { x : int } [@@deriving jsonschema] [@@allow_extra_fields]
type x_with_extra = {
  x : int;
  y : int;
}
[@@deriving jsonschema] [@@allow_extra_fields]
type 'url generic_link_traffic = {
  title : string option;
  url : 'url;
}
[@@deriving jsonschema]
type string_link_traffic = string generic_link_traffic [@@deriving jsonschema]
type 'a poly_variant =
  | A
  | B of 'a
[@@deriving jsonschema]
type ('a, 'b) multi_param = {
  first : 'a;
  second : 'b;
  label : string;
}
[@@deriving jsonschema]
type 'a param_list = 'a list [@@deriving jsonschema]
type ('a, 'b) either =
  | Left of 'a
  | Right of 'b
[@@deriving jsonschema]
type ('a, 'b) either_alias = ('a, 'b) either [@@deriving jsonschema]
type ('a, 'b) direction =
  | North
  | South
[@@deriving jsonschema ~variant_as_string]
type tool_params = {
  query : string; [@jsonschema.description "The search query to execute"]
  max_results : int; [@jsonschema.description "Maximum number of results to return"]
}
[@@deriving jsonschema]
type described_record = {
  name : string; [@jsonschema.description "The user's full name"]
  age : int option; [@jsonschema.option] [@jsonschema.description "The user's age"]
}
[@@deriving jsonschema] [@@jsonschema.description "A user object"]
type with_key_and_desc = {
  opt : int option; [@key "opt_int"] [@jsonschema.option] [@jsonschema.description "An optional integer"]
}
[@@deriving jsonschema]
type described_variant =
  | Plain [@jsonschema.description "No payload"]
  | With_int of int [@jsonschema.description "Single integer tag"]
  | Pair of string * int [@jsonschema.description "String and int"]
  | Description_value of (string[@jsonschema.description "A string value"])
[@@deriving jsonschema]
type described_variant_inline_record =
  | Point of {
      x : int;
      y : int;
    } [@jsonschema.description "A 2D point"]
[@@deriving jsonschema]
type described_variant_string =
  | A [@jsonschema.description "First choice"]
  | B [@jsonschema.description "Second choice"]
[@@deriving jsonschema ~variant_as_string]
type computation_result =
  | Ok
  | Err of string
[@@deriving jsonschema] [@@jsonschema.description "Either success or an error message string"]
type nullable_fields = {
  plain : string option;
  drop_simple : string option; [@jsonschema.option]
  drop_complex : int list option; [@jsonschema.option]
}
[@@deriving jsonschema]
type composing_type = string
let composing_type_jsonschema = `Assoc [ "type", `String "string"; "description", `String "A string" ]
type composing_record = { composing_type : composing_type option [@jsonschema.option] } [@@deriving jsonschema]
type with_format = string [@@jsonschema.format "date-time"] [@@deriving jsonschema]
type with_format_record = { with_format : string [@jsonschema.format "date-time"] } [@@deriving jsonschema]
type with_format_variant =
  | A of (string[@jsonschema.format "date-time"] [@jsonschema.description "A date-time string"])
  | B
[@@deriving jsonschema]
type 'a grade' =
  | A of 'a
  | B of ('a grade' * 'a grade')
  | C
type 'a grade = 'a grade' =
  | A of 'a
  | B of ('a grade * 'a grade)
  | C
[@@deriving jsonschema]
type self_ref = { children : self_ref list } [@@deriving jsonschema]
type two_self_refs = {
  a : self_ref;
  b : self_ref;
}
[@@deriving jsonschema]
type ('atom, 'group_atom) filter =
  | Atom of 'atom
  | Group of ('atom, 'group_atom) filter list * 'group_atom
[@@deriving jsonschema]
type ('atom, 'group_atom) bool_filter =
  | BoolAtom of ('atom, 'group_atom) filter
  | BoolFilterGroup of ('atom, 'group_atom) bool_filter list
[@@deriving jsonschema]
type 'a rec_wrapper =
  | RWrap of 'a
  | RNested of 'a rec_wrapper
[@@deriving jsonschema]
type outer_rec =
  | ORLeaf of int
  | ORNode of outer_rec rec_wrapper
[@@deriving jsonschema]
type with_maximum = int [@@jsonschema.maximum 100] [@@deriving jsonschema]
type with_maximum_record = { field : int [@jsonschema.maximum 100] } [@@deriving jsonschema]
type attrs_core_type =
  (int[@jsonschema.attrs { maximum = 100; minimum = 0; description = "Integer percentage value (0-100 inclusive)" }])
[@@deriving jsonschema]
type attrs_record = {
  score : int; [@jsonschema.attrs { maximum = 100; minimum = 0; description = "Score out of 100" }]
  label : string; [@jsonschema.attrs { format = "date-time"; description = "An ISO date-time" }]
}
[@@deriving jsonschema]
type attrs_type_decl = int [@@jsonschema.attrs { description = "A plain integer" }] [@@deriving jsonschema]
type minimum_core_type_int = (int[@jsonschema.minimum 0] [@jsonschema.maximum 100]) [@@deriving jsonschema]
type minimum_core_type_float = (float[@jsonschema.minimum 0.0] [@jsonschema.maximum 1.0]) [@@deriving jsonschema]
type minimum_maximum_record = {
  score : int; [@jsonschema.minimum 0] [@jsonschema.maximum 100]
  ratio : float; [@jsonschema.minimum 0.0] [@jsonschema.maximum 1.0]
}
[@@deriving jsonschema]
type minimum_maximum_type_decl_int = int [@@jsonschema.minimum 0] [@@jsonschema.maximum 255] [@@deriving jsonschema]
type minimum_maximum_type_decl_float = float
[@@jsonschema.minimum 0.0] [@@jsonschema.maximum 1.0] [@@deriving jsonschema]
type minimum_maximum_variant =
  | Percentage of (int[@jsonschema.minimum 0] [@jsonschema.maximum 100])
  | Factor of (float[@jsonschema.minimum 0.0] [@jsonschema.maximum 1.0])
[@@deriving jsonschema]
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
