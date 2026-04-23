let schema_version = "https://json-schema.org/draft/2020-12/schema"

(* Melange does not expose Yojson.Basic.t, and Melange_json.t is not directly
   compatible with it, so the runtime exposes its own JSON type [t]. On native,
   [t] is structurally compatible with [Yojson.Basic.t], so callers can coerce
   with [(schema :> Yojson.Basic.t)] when needed. Do not use
   [Yojson.Safe.to_basic] here: [t] is not a [Yojson.Safe.t]. *)

include struct
  type t =
    [ `Null
    | `String of string
    | `Float of float
    | `Int of int
    | `Bool of bool
    | `List of t list
    | `Assoc of (string * t) list
    ]
end

let json_schema ?id ?title ?description ?definitions (types : t) : t =
  match types with
  | `Assoc types ->
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
    `Assoc (metadata @ types)
  | _ -> invalid_arg "json_schema expects an object schema"
