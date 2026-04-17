type config = {
  variant_as_string : bool;
    (** Encode variants as string instead of string array.
        This option breaks compatibility with yojson derivers and
        doesn't support constructors with a payload. *)
  polymorphic_variant_tuple : bool;
    (** Preserve the implicit tuple in a polymorphic variant.
        This option breaks compatibility with yojson derivers. *)
}

val jsonschema_key : (Ppxlib.label_declaration, string Location.loc) Ppxlib.Attribute.t
val jsonschema_ref : (Ppxlib.label_declaration, string Location.loc) Ppxlib.Attribute.t
val jsonschema_variant_name : (Ppxlib.constructor_declaration, string Location.loc) Ppxlib.Attribute.t
val jsonschema_polymorphic_variant_name : (Ppxlib.row_field, string Location.loc) Ppxlib.Attribute.t
val jsonschema_td_allow_extra_fields : (Ppxlib.type_declaration, unit -> unit) Ppxlib.Attribute.t
val jsonschema_cd_allow_extra_fields : (Ppxlib.constructor_declaration, unit -> unit) Ppxlib.Attribute.t
val jsonschema_option : Ppxlib.label_declaration Ppxlib.Attribute.flag
val jsonschema_ld_description : (Ppxlib.label_declaration, string Location.loc) Ppxlib.Attribute.t
val jsonschema_td_description : (Ppxlib.type_declaration, string Location.loc) Ppxlib.Attribute.t
val jsonschema_cd_description : (Ppxlib.constructor_declaration, string Location.loc) Ppxlib.Attribute.t
val jsonschema_ct_description : (Ppxlib.core_type, string Location.loc) Ppxlib.Attribute.t
val jsonschema_td_format : (Ppxlib.type_declaration, string Location.loc) Ppxlib.Attribute.t
val jsonschema_ld_format : (Ppxlib.label_declaration, string Location.loc) Ppxlib.Attribute.t
val jsonschema_ct_format : (Ppxlib.core_type, string Location.loc) Ppxlib.Attribute.t
val jsonschema_td_maximum : (Ppxlib.type_declaration, Ppxlib.expression) Ppxlib.Attribute.t
val jsonschema_ld_maximum : (Ppxlib.label_declaration, Ppxlib.expression) Ppxlib.Attribute.t
val jsonschema_ct_maximum : (Ppxlib.core_type, Ppxlib.expression) Ppxlib.Attribute.t
val jsonschema_td_minimum : (Ppxlib.type_declaration, Ppxlib.expression) Ppxlib.Attribute.t
val jsonschema_ld_minimum : (Ppxlib.label_declaration, Ppxlib.expression) Ppxlib.Attribute.t
val jsonschema_ct_minimum : (Ppxlib.core_type, Ppxlib.expression) Ppxlib.Attribute.t
val jsonschema_ct_attrs : (Ppxlib.core_type, Ppxlib.expression) Ppxlib.Attribute.t
val jsonschema_td_attrs : (Ppxlib.type_declaration, Ppxlib.expression) Ppxlib.Attribute.t
val jsonschema_ld_attrs : (Ppxlib.label_declaration, Ppxlib.expression) Ppxlib.Attribute.t
val jsonschema_ld_default : (Ppxlib.label_declaration, Ppxlib.expression) Ppxlib.Attribute.t
val attributes : Ppxlib.Attribute.packed list
val args : unit -> (bool -> bool -> 'a, 'a) Ppxlib.Deriving.Args.t
