open Melange_json.Primitives

let fail name message = failwith (name ^ ": " ^ message)

let assoc name = function
  | `Assoc fields -> fields
  | _ -> fail name "expected object"

let field name fields =
  match List.assoc_opt name fields with
  | Some value -> value
  | None -> fail name "missing field"

let property schema key =
  schema
  |> Ppx_deriving_jsonschema_runtime.json_schema
  |> assoc "schema"
  |> field "properties"
  |> assoc "properties"
  |> field key

let default schema key = property schema key |> assoc key |> field "default"

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

let snapshot =
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

let float_to_json_string f =
  let s = string_of_float f in
  if String.ends_with ~suffix:"." s then s ^ "0"
  else if String.contains s '.' || String.contains s 'e' || String.contains s 'E' then s
  else s ^ ".0"

let rec json_to_string = function
  | `Null -> "null"
  | `String s -> Printf.sprintf "%S" s
  | `Float f -> float_to_json_string f
  | `Int i -> string_of_int i
  | `Bool b -> if b then "true" else "false"
  | `List xs -> "[" ^ String.concat "," (List.map json_to_string xs) ^ "]"
  | `Assoc fields ->
    let field_to_string (key, value) = Printf.sprintf "%S:%s" key (json_to_string value) in
    "{" ^ String.concat "," (List.map field_to_string fields) ^ "}"

let snapshot_string = json_to_string snapshot

let run emit =
  let expect_equal name actual expected =
    if actual = expected then emit ("ok: " ^ name) else fail name "unexpected value"
  in
  expect_equal "default_score" (default default_value_jsonschema "score") (`Int 0);
  expect_equal "default_label" (default default_value_jsonschema "label") (`String "default");
  expect_equal "default_speed" (default default_value_jsonschema "speed") (`Float 100.0);
  expect_equal "default_is_active" (default default_value_jsonschema "is_active") (`Bool false);
  expect_equal "default_pair" (default default_value_jsonschema "pair") (`List [ `Int 1; `String "hello" ]);
  expect_equal "default_pairs"
    (default default_value_jsonschema "pairs")
    (`List [ `List [ `String "a"; `Null ]; `List [ `String "b"; `String "b" ] ]);
  expect_equal "default_variant" (default default_value_jsonschema "variant") (`List [ `String "A" ]);
  expect_equal "default_record" (default default_value_jsonschema "record") (`Assoc [ "score", `Null ]);
  expect_equal "default_int_list" (default default_value_jsonschema "int_list") (`List [ `Int 1; `Int 2; `Int 3 ]);
  expect_equal "default_empty_list" (default default_value_jsonschema "empty_list") (`List []);
  expect_equal "module_t_default" (default default_with_module_type_jsonschema "status") (`List [ `String "Active" ])
