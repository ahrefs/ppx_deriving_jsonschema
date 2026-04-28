let schema_version = "https://json-schema.org/draft/2020-12/schema"

type t = Ppx_deriving_jsonschema_runtime_classify.t

let classify = Ppx_deriving_jsonschema_runtime_classify.classify

let json_schema ?id ?title ?description ?definitions types =
  let fields =
    match types with
    | `Assoc fields -> fields
    | _ -> []
  in
  let metadata =
    List.filter_map
      (fun x -> x)
      [
        Some ("$schema", `String schema_version);
        (match id with
        | None -> None
        | Some id -> Some ("$id", `String id));
        (match title with
        | None -> None
        | Some title -> Some ("title", `String title));
        (match description with
        | None -> None
        | Some description -> Some ("description", `String description));
        (match definitions with
        | None -> None
        | Some defs -> Some ("$defs", `Assoc defs));
      ]
  in
  `Assoc (metadata @ fields)

module Primitives = Ppx_deriving_jsonschema_runtime_primitives
