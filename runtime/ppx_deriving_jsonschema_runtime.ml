let schema_version = "https://json-schema.org/draft/2020-12/schema"

let json_schema typ =
  match typ with
  | `Assoc l -> `Assoc (("$schema", `String schema_version) :: l)
