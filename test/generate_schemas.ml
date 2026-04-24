let yojson_of_json (json : Ppx_deriving_jsonschema_runtime.t) : Yojson.Basic.t = (json :> Yojson.Basic.t)

let () = print_endline (Yojson.Basic.pretty_to_string (yojson_of_json Generate_schemas_cases.snapshot))
