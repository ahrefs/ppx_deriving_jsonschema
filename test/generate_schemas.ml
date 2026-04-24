let rec yojson_of_json = function
  | `Null -> `Null
  | `String s -> `String s
  | `Float f -> `Float f
  | `Int i -> `Int i
  | `Bool b -> `Bool b
  | `List xs -> `List (List.map yojson_of_json xs)
  | `Assoc fields -> `Assoc (List.map (fun (key, value) -> key, yojson_of_json value) fields)

let () = print_endline (Yojson.Basic.pretty_to_string (yojson_of_json Generate_schemas_cases.snapshot))
