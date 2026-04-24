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

let expect_equal name actual expected =
  if actual = expected then Printf.printf "ok: %s\n" name else fail name "unexpected value"

type status =
  | Active
  | Inactive
[@@deriving to_json, jsonschema]

type record_for_default = { score : int option } [@@deriving to_json, jsonschema]

type derived_defaults = {
  status : status; [@jsonschema.default Active]
  record : record_for_default; [@jsonschema.default { score = None }]
}
[@@deriving jsonschema]

module Scoped = struct
  module Status = struct
    type t =
      | Active
      | Inactive
    [@@deriving to_json, jsonschema]
  end

  type with_module_type = { status : Status.t [@jsonschema.default Status.Active] } [@@deriving jsonschema]
end

let () =
  expect_equal "derived_variant_default" (default derived_defaults_jsonschema "status") (`List [ `String "Active" ]);
  expect_equal "derived_record_default" (default derived_defaults_jsonschema "record") (`Assoc [ "score", `Null ]);
  expect_equal "module_t_default" (default Scoped.with_module_type_jsonschema "status") (`List [ `String "Active" ])
