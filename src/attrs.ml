open Ppxlib

type config = {
  variant_as_string : bool;
    (** Encode variants as string instead of string array. This option breaks compatibility with yojson derivers and
        doesn't support constructors with a payload. *)
  polymorphic_variant_tuple : bool;
    (** Preserve the implicit tuple in a polymorphic variant. This option breaks compatibility with yojson derivers. *)
  ocaml_doc : bool;
    (** Use [ocaml.doc] attributes (i.e. [(** ... *)] comments) as a fallback for [[@jsonschema.description]] when the
        explicit annotation is absent. *)
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
let jsonschema_rtag_description = string_attr "jsonschema.description" Attribute.Context.rtag

let jsonschema_td_format = string_attr "jsonschema.format" Attribute.Context.type_declaration
let jsonschema_ld_format = string_attr "jsonschema.format" Attribute.Context.label_declaration
let jsonschema_ct_format = string_attr "jsonschema.format" Attribute.Context.core_type

let jsonschema_td_maximum = expr_attr "jsonschema.maximum" Attribute.Context.type_declaration
let jsonschema_ld_maximum = expr_attr "jsonschema.maximum" Attribute.Context.label_declaration
let jsonschema_ct_maximum = expr_attr "jsonschema.maximum" Attribute.Context.core_type

let jsonschema_td_minimum = expr_attr "jsonschema.minimum" Attribute.Context.type_declaration
let jsonschema_ld_minimum = expr_attr "jsonschema.minimum" Attribute.Context.label_declaration
let jsonschema_ct_minimum = expr_attr "jsonschema.minimum" Attribute.Context.core_type

let jsonschema_ct_attrs = expr_attr "jsonschema.attrs" Attribute.Context.core_type
let jsonschema_td_attrs = expr_attr "jsonschema.attrs" Attribute.Context.type_declaration
let jsonschema_ld_attrs = expr_attr "jsonschema.attrs" Attribute.Context.label_declaration
let jsonschema_ld_default = expr_attr "jsonschema.default" Attribute.Context.label_declaration

(* We intentionally do not use [Attribute.get] for [ocaml.doc]/[doc]. These are
   compiler-reserved attributes, and [ppxlib] rejects registering them via
   [Attribute.declare]. We therefore inspect the raw attribute list directly
   with an [Ast_pattern] that matches both the name and the standard string
   payload shape in one go. *)
let doc_attr_pattern =
  Ast_pattern.(attribute ~name:(string "ocaml.doc" ||| string "doc") ~payload:(single_expr_payload (estring __')))

(* A node can carry several [ocaml.doc]/[doc] attributes — e.g. a user writing
   one doc comment before a record field and another after. We collect every
   match and join them with a blank line so each comment reads as its own
   paragraph. The returned location is that of the first matching attribute. *)
let find_doc_attr attrs =
  let matches =
    List.filter_map
      (fun attr ->
        Ast_pattern.parse_res doc_attr_pattern attr.attr_loc attr Fun.id
        |> Result.to_option
        |> Option.map (fun ({ txt; loc } : string Location.loc) -> { txt = String.trim txt; loc }))
      attrs
  in
  match matches with
  | [] -> None
  | [ single ] -> Some single
  | first :: _ as all -> Some { txt = String.concat "\n\n" (List.map (fun x -> x.txt) all); loc = first.loc }

let fallback_description ~ocaml_doc explicit_desc attrs node =
  match Attribute.get explicit_desc node with
  | Some _ as x -> x
  | None -> if ocaml_doc then find_doc_attr attrs else None

let ld_description ~ocaml_doc (ld : label_declaration) =
  fallback_description ~ocaml_doc jsonschema_ld_description ld.pld_attributes ld

let td_description ~ocaml_doc (td : type_declaration) =
  fallback_description ~ocaml_doc jsonschema_td_description td.ptype_attributes td

let cd_description ~ocaml_doc (cd : constructor_declaration) =
  fallback_description ~ocaml_doc jsonschema_cd_description cd.pcd_attributes cd

let ct_description ~ocaml_doc (ct : core_type) =
  fallback_description ~ocaml_doc jsonschema_ct_description ct.ptyp_attributes ct

let rtag_description ~ocaml_doc (rf : row_field) =
  fallback_description ~ocaml_doc jsonschema_rtag_description rf.prf_attributes rf

let jsonschema_td_compact_variants =
  Attribute.declare_flag "jsonschema.compact_variants" Attribute.Context.type_declaration

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
    Attribute.T jsonschema_rtag_description;
    Attribute.T jsonschema_td_format;
    Attribute.T jsonschema_ld_format;
    Attribute.T jsonschema_ct_format;
    Attribute.T jsonschema_td_maximum;
    Attribute.T jsonschema_ld_maximum;
    Attribute.T jsonschema_ct_maximum;
    Attribute.T jsonschema_td_minimum;
    Attribute.T jsonschema_ld_minimum;
    Attribute.T jsonschema_ct_minimum;
    Attribute.T jsonschema_ct_attrs;
    Attribute.T jsonschema_td_attrs;
    Attribute.T jsonschema_ld_attrs;
    Attribute.T jsonschema_ld_default;
    Attribute.T jsonschema_td_compact_variants;
  ]

let args () = Deriving.Args.(empty +> flag "variant_as_string" +> flag "polymorphic_variant_tuple" +> flag "ocaml_doc")
