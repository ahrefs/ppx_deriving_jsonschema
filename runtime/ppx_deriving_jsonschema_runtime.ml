let schema_version = "https://json-schema.org/draft/2020-12/schema"

let json_schema ?id ?title ?description ?definitions types =
  match types with
  | `Assoc types ->
    let embedded, types =
      List.partition_map
        (function
          | "$defs", `Assoc defs -> Left defs
          | entry -> Right entry)
        types
    in
    let defs = List.concat embedded @ Option.value ~default:[] definitions in
    let opt k v = Option.to_list (Option.map (fun v -> k, `String v) v) in
    let metadata =
      ("$schema", `String schema_version)
      :: opt "$id" id
      @ opt "title" title
      @ opt "description" description
      @ (match defs with [] -> [] | defs -> [ "$defs", `Assoc defs ])
    in
    `Assoc (metadata @ types)
