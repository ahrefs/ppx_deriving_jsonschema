open Test
open Ppx_deriving_jsonschema_runtime

let schemas =
  [
    json_schema with_modules_jsonschema;
    json_schema kind_jsonschema;
    json_schema kind_as_array_jsonschema;
    json_schema poly_kind_jsonschema;
    json_schema poly_kind_as_array_jsonschema;
    json_schema poly_kind_with_payload_jsonschema;
    json_schema poly_kind_with_payload_as_array_jsonschema;
    json_schema poly_inherit_jsonschema;
    json_schema poly_inherit_as_array_jsonschema;
    json_schema event_jsonschema;
    json_schema events_jsonschema;
    json_schema eventss_jsonschema;
    json_schema event_comment_jsonschema;
    json_schema event_comments'_jsonschema;
    json_schema event_n_jsonschema;
    json_schema events_array_jsonschema;
    json_schema numbers_jsonschema;
    json_schema opt_jsonschema;
    json_schema using_m_jsonschema;
    json_schema poly2_jsonschema;
    json_schema tuple_with_variant_jsonschema;
    json_schema ~id:"https://ahrefs.com/schemas/player_scores" ~title:"Player scores"
      ~description:"Object representing player scores"
      ~definitions:[ "numbers", numbers_jsonschema ]
      player_scores_jsonschema;
    json_schema t_jsonschema;
    json_schema ~definitions:[ "shared_address", address_jsonschema ] tt_jsonschema;
    json_schema c_jsonschema;
    json_schema variant_inline_record_jsonschema;
    json_schema variant_with_payload_jsonschema;
  ]

let schema = `Assoc [ "$schema", `String "https://json-schema.org/draft/2020-12/schema"; "oneOf", `List schemas ]

let () = print_endline (Yojson.Basic.pretty_to_string schema)
