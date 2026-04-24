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

let pretty_indent depth = String.make (depth * 2) ' '

let rec pretty_json_to_string_aux depth = function
  | (`Null | `String _ | `Float _ | `Int _ | `Bool _) as json -> json_to_string json
  | `List [] -> "[]"
  | `List xs ->
    let items = List.map (fun value -> pretty_indent (depth + 1) ^ pretty_json_to_string_aux (depth + 1) value) xs in
    "[\n" ^ String.concat ",\n" items ^ "\n" ^ pretty_indent depth ^ "]"
  | `Assoc [] -> "{}"
  | `Assoc fields ->
    let fields =
      List.map
        (fun (key, value) ->
          Printf.sprintf "%s%S: %s" (pretty_indent (depth + 1)) key (pretty_json_to_string_aux (depth + 1) value))
        fields
    in
    "{\n" ^ String.concat ",\n" fields ^ "\n" ^ pretty_indent depth ^ "}"

let pretty_json_to_string json = pretty_json_to_string_aux 0 json
