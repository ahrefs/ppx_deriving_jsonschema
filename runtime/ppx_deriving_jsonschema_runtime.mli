val schema_version : string

type t = Ppx_deriving_jsonschema_runtime_classify.t

val classify : t -> t [@@platform native]
val classify : Js.Json.t -> t [@@platform js]
val declassify : t -> t [@@platform native]
val declassify : t -> Js.Json.t [@@platform js]

val json_schema : ?id:string -> ?title:string -> ?description:string -> ?definitions:(string * t) list -> t -> t

module Primitives : module type of Ppx_deriving_jsonschema_runtime_primitives
