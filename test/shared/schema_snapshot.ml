(* Shared schema snapshots are built as [Ppx_deriving_jsonschema_runtime.t] so the
   same OCaml code can be reused by native tests and Melange tests.

   Native tests can coerce that type to [Yojson.Basic.t] at the printing boundary,
   but Melange has no [Yojson.Basic]. We therefore keep a tiny target-neutral JSON
   serializer here so both targets can print the exact same shared snapshot value. *)

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
