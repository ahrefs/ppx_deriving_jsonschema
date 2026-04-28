open Cases

(* [shared/cases.ml] is the source of truth for the case definitions.
   This module only enumerates which generated schemas are included in the shared
   native/Melange snapshot. *)

let schemas =
  [
    Ppx_deriving_jsonschema_runtime.json_schema Mod1.m_1_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema Mod1.Mod2.m_2_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema with_modules_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema kind_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema kind_as_string_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_kind_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_kind_as_string_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_kind_with_payload_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_kind_with_payload_as_string_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_inherit_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema poly_inherit_as_string_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema event_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema recursive_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema recursive_variant_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema tree_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema non_recursive_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema foo_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema bar_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema expr_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema alpha_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema beta_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema node_a_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema recursive_tuple_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema int_tree_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema events_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema eventss_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema event_comment_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema event_comments'_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema event_n_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema events_array_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema numbers_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema opt_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema using_m_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema (poly2_jsonschema int_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema tuple_with_variant_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema ~id:"https://ahrefs.com/schemas/player_scores" ~title:"Player scores"
      ~description:"Object representing player scores"
      ~definitions:[ "numbers", numbers_jsonschema ]
      player_scores_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema ~definitions:[ "shared_address", address_jsonschema ] tt_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema c_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema variant_inline_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema inline_record_with_extra_fields_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema variant_with_payload_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t1_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t2_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t3_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t4_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t5_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t6_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t7_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t8_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t9_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t10_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema t11_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema nested_obj_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema x_without_extra_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema (generic_link_traffic_jsonschema string_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema string_link_traffic_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema (poly_variant_jsonschema int_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema (multi_param_jsonschema int_jsonschema bool_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema (param_list_jsonschema string_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema (either_jsonschema int_jsonschema string_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema (either_alias_jsonschema int_jsonschema string_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema (direction_jsonschema int_jsonschema string_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema tool_params_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema described_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema with_key_and_desc_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema described_variant_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema described_variant_inline_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema described_variant_string_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_comment_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_comment_disabled_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_comment_override_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_comment_variant_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_comment_core_type_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_attribute_alias_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_comment_multiline_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_comment_poly_variant_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_comment_multiple_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema doc_comment_poly_variant_override_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema computation_result_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema nullable_fields_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema melange_json_defaults_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema composing_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema with_format_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema with_format_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema with_format_variant_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema (grade_jsonschema int_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema two_self_refs_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema (filter_jsonschema int_jsonschema string_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema outer_rec_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema (bool_filter_jsonschema int_jsonschema string_jsonschema);
    Ppx_deriving_jsonschema_runtime.json_schema with_maximum_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema with_maximum_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema attrs_core_type_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema attrs_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema attrs_type_decl_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema minimum_core_type_int_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema minimum_core_type_float_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema minimum_maximum_record_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema minimum_maximum_type_decl_int_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema minimum_maximum_type_decl_float_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema minimum_maximum_variant_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema default_value_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema default_with_module_type_jsonschema;
    Ppx_deriving_jsonschema_runtime.json_schema compact_variants_jsonschema;
  ]

let snapshot = `Assoc [ "$schema", `String Ppx_deriving_jsonschema_runtime.schema_version; "oneOf", `List schemas ]

let snapshot_string = Schema_snapshot.json_to_string snapshot
