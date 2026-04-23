type config = {
  variant_as_string : bool;
    (** Encode variants as string instead of string array.
        This option breaks compatibility with yojson derivers and
        doesn't support constructors with a payload. *)
  polymorphic_variant_tuple : bool;
    (** Preserve the implicit tuple in a polymorphic variant.
        This option breaks compatibility with yojson derivers. *)
  ocaml_doc : bool;
    (** Use [ocaml.doc] attributes (i.e. [(** ... *)] comments) as a fallback
        for [\[@jsonschema.description\]] when the explicit annotation is absent. *)
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
val jsonschema_rtag_description : (Ppxlib.row_field, string Location.loc) Ppxlib.Attribute.t
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

(** [ld_description], [td_description], [cd_description], [ct_description] and
    [rtag_description] resolve a description from [\[@jsonschema.description "..."\]].
    When [ocaml_doc] is [true] and the explicit annotation is absent, they fall back
    to an [ocaml.doc] attribute (i.e. a [(** ... *)] comment) on the same node. *)
val ld_description : ocaml_doc:bool -> Ppxlib.label_declaration -> string Location.loc option

val td_description : ocaml_doc:bool -> Ppxlib.type_declaration -> string Location.loc option
val cd_description : ocaml_doc:bool -> Ppxlib.constructor_declaration -> string Location.loc option
val ct_description : ocaml_doc:bool -> Ppxlib.core_type -> string Location.loc option
val rtag_description : ocaml_doc:bool -> Ppxlib.row_field -> string Location.loc option
val attributes : Ppxlib.Attribute.packed list
val args : unit -> (bool -> bool -> bool -> 'a, 'a) Ppxlib.Deriving.Args.t
