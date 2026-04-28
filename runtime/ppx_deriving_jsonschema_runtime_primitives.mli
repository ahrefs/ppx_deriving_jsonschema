module Yojson : sig
  type t = Ppx_deriving_jsonschema_runtime_classify.t
  val char_jsonschema : t
  val string_jsonschema : t
  val bool_jsonschema : t
  val float_jsonschema : t
  val int_jsonschema : t
  val option_jsonschema : t -> t
  val unit_jsonschema : t
  val list_jsonschema : t -> t
  val array_jsonschema : t -> t
  val int64_jsonschema : t
end
[@@platform native]

module Melange_json : sig
  type t = Ppx_deriving_jsonschema_runtime_classify.t
  val char_jsonschema : t
  val string_jsonschema : t
  val bool_jsonschema : t
  val float_jsonschema : t
  val int_jsonschema : t
  val option_jsonschema : t -> t
  val unit_jsonschema : t
  val list_jsonschema : t -> t
  val array_jsonschema : t -> t
  val int64_jsonschema : t
  val result_jsonschema : t -> t -> t
end
