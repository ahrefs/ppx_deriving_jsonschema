module Common = struct
  type t = Ppx_deriving_jsonschema_runtime_classify.t
  let char_jsonschema : t = `Assoc [ "type", `String "string"; "minLength", `Int 1; "maxLength", `Int 1 ]
  let string_jsonschema : t = `Assoc [ "type", `String "string" ]
  let bool_jsonschema : t = `Assoc [ "type", `String "boolean" ]
  let float_jsonschema : t = `Assoc [ "type", `String "number" ]
  let int_jsonschema : t = `Assoc [ "type", `String "integer" ]
  let option_jsonschema (schema : t) : t =
    let is_basic_type ty = ty = "string" || ty = "number" || ty = "boolean" || ty = "integer" in
    match schema with
    | `Assoc (("type", `String ty) :: rest) when is_basic_type ty ->
      `Assoc (("type", `List [ `String ty; `String "null" ]) :: rest)
    | s -> `Assoc [ "anyOf", `List [ s; `Assoc [ "type", `String "null" ] ] ]
  let unit_jsonschema : t = `Assoc [ "type", `String "null" ]
  let list_jsonschema element_type : t = `Assoc [ "type", `String "array"; "items", element_type ]
  let array_jsonschema element_type : t = `Assoc [ "type", `String "array"; "items", element_type ]
end

module Yojson = struct
  include Common
  let int64_jsonschema : t = `Assoc [ "type", `String "integer" ]
end
[@@platform native]

module Melange_json = struct
  include Common
  let int64_jsonschema : t =
    `Assoc [ "type", `String "string"; "description", `String "int64 is represented as a string" ]
  let result_jsonschema (error_schema : t) (ok_schema : t) : t =
    `Assoc
      [
        ( "anyOf",
          `List
            [
              `Assoc
                [
                  "type", `String "array";
                  "prefixItems", `List [ `Assoc [ "const", `String "Error" ]; error_schema ];
                  "unevaluatedItems", `Bool false;
                  "minItems", `Int 1;
                ];
              `Assoc
                [
                  "type", `String "array";
                  "prefixItems", `List [ `Assoc [ "const", `String "Ok" ]; ok_schema ];
                  "unevaluatedItems", `Bool false;
                  "minItems", `Int 2;
                  "maxItems", `Int 2;
                ];
            ] );
      ]
end
