open Ppxlib

type config = {
  variant_as_string : bool;
    (** Encode variants as string instead of string array.
        This option breaks compatibility with yojson derivers and
        doesn't support constructors with a payload. *)
  polymorphic_variant_tuple : bool;
    (** Preserve the implicit tuple in a polymorphic variant.
        This option breaks compatibility with yojson derivers. *)
}

let string_attr name ctx = Attribute.declare name ctx Ast_pattern.(single_expr_payload (estring __')) (fun x -> x)

let expr_attr name ctx = Attribute.declare name ctx Ast_pattern.(single_expr_payload __) (fun x -> x)

let jsonschema_key = string_attr "jsonschema.key" Attribute.Context.label_declaration
let jsonschema_ref = string_attr "jsonschema.ref" Attribute.Context.label_declaration
let jsonschema_variant_name = string_attr "jsonschema.name" Attribute.Context.constructor_declaration
let jsonschema_polymorphic_variant_name = string_attr "jsonschema.name" Attribute.Context.rtag

let jsonschema_td_allow_extra_fields =
  Attribute.declare "jsonschema.allow_extra_fields" Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    (fun () -> ())

let jsonschema_cd_allow_extra_fields =
  Attribute.declare "jsonschema.allow_extra_fields" Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    (fun () -> ())

let jsonschema_option = Attribute.declare_flag "jsonschema.option" Attribute.Context.label_declaration

let jsonschema_ld_description = string_attr "jsonschema.description" Attribute.Context.label_declaration
let jsonschema_td_description = string_attr "jsonschema.description" Attribute.Context.type_declaration
let jsonschema_cd_description = string_attr "jsonschema.description" Attribute.Context.constructor_declaration
let jsonschema_ct_description = string_attr "jsonschema.description" Attribute.Context.core_type

let jsonschema_td_format = string_attr "jsonschema.format" Attribute.Context.type_declaration
let jsonschema_ld_format = string_attr "jsonschema.format" Attribute.Context.label_declaration
let jsonschema_ct_format = string_attr "jsonschema.format" Attribute.Context.core_type

let jsonschema_td_maximum = expr_attr "jsonschema.maximum" Attribute.Context.type_declaration
let jsonschema_ld_maximum = expr_attr "jsonschema.maximum" Attribute.Context.label_declaration
let jsonschema_ct_maximum = expr_attr "jsonschema.maximum" Attribute.Context.core_type

let jsonschema_td_minimum = expr_attr "jsonschema.minimum" Attribute.Context.type_declaration
let jsonschema_ld_minimum = expr_attr "jsonschema.minimum" Attribute.Context.label_declaration
let jsonschema_ct_minimum = expr_attr "jsonschema.minimum" Attribute.Context.core_type

let attributes =
  [
    Attribute.T jsonschema_key;
    Attribute.T jsonschema_ref;
    Attribute.T jsonschema_variant_name;
    Attribute.T jsonschema_polymorphic_variant_name;
    Attribute.T jsonschema_td_allow_extra_fields;
    Attribute.T jsonschema_cd_allow_extra_fields;
    Attribute.T jsonschema_option;
    Attribute.T jsonschema_ld_description;
    Attribute.T jsonschema_td_description;
    Attribute.T jsonschema_cd_description;
    Attribute.T jsonschema_ct_description;
    Attribute.T jsonschema_td_format;
    Attribute.T jsonschema_ld_format;
    Attribute.T jsonschema_ct_format;
    Attribute.T jsonschema_td_maximum;
    Attribute.T jsonschema_ld_maximum;
    Attribute.T jsonschema_ct_maximum;
    Attribute.T jsonschema_td_minimum;
    Attribute.T jsonschema_ld_minimum;
    Attribute.T jsonschema_ct_minimum;
  ]

let args () = Deriving.Args.(empty +> flag "variant_as_string" +> flag "polymorphic_variant_tuple")
