open Ppxlib
open Ast_builder.Default

type config = {
  variant_as_string : bool;
    (** Encode variants as string instead of string array.
        This option breaks compatibility with yojson derivers and
        doesn't support constructors with a payload. *)
  polymorphic_variant_tuple : bool;
    (** Preserve the implicit tuple in a polymorphic variant.
        This option breaks compatibility with yojson derivers. *)
}

let deriver_name = "jsonschema"

let jsonschema_key =
  Attribute.declare "jsonschema.key" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __'))
    (fun x -> x)

let jsonschema_ref =
  Attribute.declare "jsonschema.ref" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload (estring __'))
    (fun x -> x)

let jsonschema_variant_name =
  Attribute.declare "jsonschema.name" Attribute.Context.constructor_declaration
    Ast_pattern.(single_expr_payload (estring __'))
    (fun x -> x)

let jsonschema_polymorphic_variant_name =
  Attribute.declare "jsonschema.name" Attribute.Context.rtag
    Ast_pattern.(single_expr_payload (estring __'))
    (fun x -> x)

let jsonschema_td_allow_extra_fields =
  Attribute.declare "jsonschema.allow_extra_fields" Attribute.Context.type_declaration
    Ast_pattern.(pstr nil)
    (fun () -> ())

let jsonschema_cd_allow_extra_fields =
  Attribute.declare "jsonschema.allow_extra_fields" Attribute.Context.constructor_declaration
    Ast_pattern.(pstr nil)
    (fun () -> ())

let attributes =
  [
    Attribute.T jsonschema_key;
    Attribute.T jsonschema_ref;
    Attribute.T jsonschema_variant_name;
    Attribute.T jsonschema_polymorphic_variant_name;
    Attribute.T jsonschema_td_allow_extra_fields;
    Attribute.T jsonschema_cd_allow_extra_fields;
  ]

(* let args () = Deriving.Args.(empty) *)
let args () = Deriving.Args.(empty +> flag "variant_as_string" +> flag "polymorphic_variant_tuple")

let deps = []

let predefined_types = [ "string"; "int"; "float"; "bool" ]
let is_predefined_type type_name = List.mem type_name predefined_types

module Schema = struct
  let const ~loc value = [%expr `Assoc [ "const", `String [%e estring ~loc value] ]]

  let type_ref ~loc type_name =
    let name = estring ~loc ("#/$defs/" ^ type_name) in
    [%expr `Assoc [ "$ref", `String [%e name] ]]

  let type_def ~loc type_name = [%expr `Assoc [ "type", `String [%e estring ~loc type_name] ]]

  let null ~loc = [%expr `Assoc [ "type", `String "null" ]]

  let char ~loc = [%expr `Assoc [ "type", `String "string"; "minLength", `Int 1; "maxLength", `Int 1 ]]

  let oneOf ~loc values = [%expr `Assoc [ "oneOf", `List [%e elist ~loc values] ]]

  let anyOf ~loc values = [%expr `Assoc [ "anyOf", `List [%e elist ~loc values] ]]

  let array_ ~loc ?min_items ?max_items element_type =
    let fields =
      List.filter_map
        (fun x -> x)
        [
          Some [%expr "type", `String "array"];
          Some [%expr "items", [%e element_type]];
          (match min_items with
          | Some min -> Some [%expr "minItems", `Int [%e eint ~loc min]]
          | None -> None);
          (match max_items with
          | Some max -> Some [%expr "maxItems", `Int [%e eint ~loc max]]
          | None -> None);
        ]
    in
    [%expr `Assoc [%e elist ~loc fields]]

  let tuple ~loc elements =
    [%expr
      `Assoc
        [
          "type", `String "array";
          "prefixItems", `List [%e elist ~loc elements];
          "unevaluatedItems", `Bool false;
          "minItems", `Int [%e eint ~loc (List.length elements)];
          "maxItems", `Int [%e eint ~loc (List.length elements)];
        ]]

  let enum ~loc typ values =
    match typ with
    | Some typ -> [%expr `Assoc [ "type", `String [%e estring ~loc typ]; "enum", `List [%e elist ~loc values] ]]
    | None -> [%expr `Assoc [ "enum", `List [%e elist ~loc values] ]]

  let enum_string ~loc values =
    let values = List.map (fun name -> [%expr `String [%e estring ~loc name]]) values in
    enum ~loc (Some "string") values

  let with_defs ~loc type_name schema =
    [%expr
      `Assoc
        [
          "$defs", `Assoc [ [%e estring ~loc type_name], [%e schema] ];
          "$ref", `String [%e estring ~loc ("#/$defs/" ^ type_name)];
        ]]

  (* For mutually recursive types: multiple definitions, ref to first type *)
  let with_multi_defs ~loc ~primary_type defs =
    let defs_expr = elist ~loc (List.map (fun (name, schema) -> [%expr [%e estring ~loc name], [%e schema]]) defs) in
    [%expr `Assoc [ "$defs", `Assoc [%e defs_expr]; "$ref", `String [%e estring ~loc ("#/$defs/" ^ primary_type)] ]]
end

let variant_as_string ~loc constrs =
  Schema.anyOf ~loc
    (List.map
       (function
         | `Tag (name, _typs) -> Schema.const ~loc name
         | `Inherit typ -> typ)
       constrs)

let variant_as_array ~loc constrs =
  Schema.anyOf ~loc
    (List.map
       (function
         | `Tag (name, typs) -> Schema.tuple ~loc (Schema.const ~loc name :: typs)
         | `Inherit typ -> typ)
       constrs)

let variant ~loc ~config constrs =
  Schema.anyOf ~loc
    (List.map
       (function
         | `Inherit typ -> typ
         | `Tag (name, typs) ->
         match config.variant_as_string with
         | true -> Schema.const ~loc name
         | false -> Schema.tuple ~loc (Schema.const ~loc name :: typs))
       constrs)

let value_name_pattern ~loc type_name = ppat_var ~loc { txt = type_name ^ "_jsonschema"; loc }

let create_value ~loc name value =
  [%stri let[@warning "-32-39"] (* rec *) [%p value_name_pattern ~loc name (* : [< `Assoc of _ list ] *)] = [%e value]]

let is_optional_type core_type =
  match core_type with
  | [%type: [%t? _] option] -> true
  | _ -> false

(* Returns (schema_expression, is_recursive)
   recursive_types: list of type names in a mutually recursive group *)
let rec type_of_core ~config ?(recursive_types = []) core_type =
  let loc = core_type.ptyp_loc in
  match core_type with
  | [%type: int] | [%type: int32] | [%type: int64] | [%type: nativeint] -> Schema.type_def ~loc "integer", false
  | [%type: float] -> Schema.type_def ~loc "number", false
  | [%type: string] | [%type: bytes] -> Schema.type_def ~loc "string", false
  | [%type: bool] -> Schema.type_def ~loc "boolean", false
  | [%type: char] -> Schema.char ~loc, false
  | [%type: unit] -> Schema.null ~loc, false
  | [%type: [%t? t] option] -> type_of_core ~config ~recursive_types t
  | [%type: [%t? t] ref] -> type_of_core ~config ~recursive_types t
  | [%type: [%t? t] list] | [%type: [%t? t] array] ->
    let t, is_rec = type_of_core ~config ~recursive_types t in
    Schema.array_ ~loc t, is_rec
  | _ ->
  match core_type.ptyp_desc with
  | Ptyp_constr (id, []) ->
    (* Check if this references a type in the recursive group *)
    (match id.txt with
    | Lident name when List.mem name recursive_types ->
      (* This is a recursive reference - use $ref *)
      Schema.type_ref ~loc name, true
    | _ ->
      (* Not recursive - inline as before *)
      type_constr_conv ~loc id ~f:(fun s -> s ^ "_jsonschema") [], false)
  | Ptyp_tuple types ->
    let results = List.map (type_of_core ~config ~recursive_types) types in
    let ts = List.map fst results in
    let is_rec = List.exists snd results in
    Schema.tuple ~loc ts, is_rec
  | Ptyp_variant (row_fields, _, _) ->
    let constrs, is_rec =
      List.fold_left
        (fun (constrs, is_rec) row_field ->
          match row_field.prf_desc with
          | Rtag (name, true, []) ->
            let name =
              match Attribute.get jsonschema_polymorphic_variant_name row_field with
              | Some name -> name.txt
              | None -> name.txt
            in
            `Tag (name, []) :: constrs, is_rec
          | Rtag (name, false, [ typ ]) ->
            let name =
              match Attribute.get jsonschema_polymorphic_variant_name row_field with
              | Some name -> name.txt
              | None -> name.txt
            in
            let raw_typs =
              match config.polymorphic_variant_tuple with
              | true -> [ typ ]
              | false ->
              match typ.ptyp_desc with
              | Ptyp_tuple tps -> tps
              | _ -> [ typ ]
            in
            let results = List.map (type_of_core ~config ~recursive_types) raw_typs in
            let typs = List.map fst results in
            let typs_rec = List.exists snd results in
            `Tag (name, typs) :: constrs, is_rec || typs_rec
          | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
            Location.raise_errorf ~loc "ppx_deriving_jsonschema: polymorphic_variant/Rtag/&"
          | Rinherit core_type ->
            let typ, typ_rec = type_of_core ~config ~recursive_types core_type in
            `Inherit typ :: constrs, is_rec || typ_rec
          (* impossible?*)
          | Rtag (_, false, []) -> assert false)
        ([], false) row_fields
    in
    let constrs = List.rev constrs in
    (* todo: raise an error if encoding is as string and constructor has a payload *)
    let v =
      match config.variant_as_string with
      | true -> variant_as_string ~loc constrs
      | false -> variant_as_array ~loc constrs
    in
    v, is_rec
  | _ ->
    let msg = Format.asprintf "ppx_deriving_jsonschema: unsupported type %a" Astlib.Pprintast.core_type core_type in
    [%expr [%ocaml.error [%e estring ~loc msg]]], false

(* Returns (schema_expression, is_recursive) *)
let object_ ~loc ~config ?(recursive_types = []) fields allow_extra_fields =
  let fields, required, is_rec =
    List.fold_left
      (fun (fields, required, is_rec) ({ pld_name; pld_type; pld_loc = _loc; _ } as field) ->
        let name =
          match Attribute.get jsonschema_key field with
          | Some name -> name.txt
          | None -> pld_name.txt
        in
        let type_def, field_rec =
          match Attribute.get jsonschema_ref field with
          | Some def -> Schema.type_ref ~loc def.txt, false
          | None -> type_of_core ~config ~recursive_types pld_type
        in
        ( [%expr [%e estring ~loc name], [%e type_def]] :: fields,
          (if is_optional_type pld_type then required else { txt = name; loc } :: required),
          is_rec || field_rec ))
      ([], [], false) fields
  in
  let required = List.map (fun { txt = name; loc } -> [%expr `String [%e estring ~loc name]]) required in
  ( [%expr
      `Assoc
        [
          "type", `String "object";
          "properties", `Assoc [%e elist ~loc fields];
          "required", `List [%e elist ~loc required];
          "additionalProperties", `Bool [%e ebool ~loc allow_extra_fields];
        ]],
    is_rec )

(* Generate schema for a single type declaration, given the recursive type group *)
let derive_single_type ~loc ~config ~recursive_types type_decl =
  let type_name = type_decl.ptype_name.txt in
  let allow_extra_fields = Attribute.get jsonschema_td_allow_extra_fields type_decl |> Option.is_some in
  match type_decl.ptype_kind with
  | Ptype_variant variants ->
    let variants, is_rec =
      List.fold_left
        (fun (variants, is_rec) ({ pcd_args; pcd_name = { txt = name; _ }; _ } as var) ->
          let name =
            match Attribute.get jsonschema_variant_name var with
            | Some name -> name.txt
            | None -> name
          in
          match pcd_args with
          | Pcstr_record label_declarations ->
            let allow_extra_fields = Attribute.get jsonschema_cd_allow_extra_fields var |> Option.is_some in
            let obj_schema, obj_rec = object_ ~loc ~config ~recursive_types label_declarations allow_extra_fields in
            `Tag (name, [ obj_schema ]) :: variants, is_rec || obj_rec
          | Pcstr_tuple typs ->
            let results = List.map (type_of_core ~config ~recursive_types) typs in
            let types = List.map fst results in
            let typs_rec = List.exists snd results in
            `Tag (name, types) :: variants, is_rec || typs_rec)
        ([], false) variants
    in
    let variants = List.rev variants in
    let schema =
      match config.variant_as_string with
      | true -> variant_as_string ~loc variants
      | false -> variant_as_array ~loc variants
    in
    type_name, schema, is_rec
  | Ptype_record label_declarations ->
    let schema, is_rec = object_ ~loc ~config ~recursive_types label_declarations allow_extra_fields in
    type_name, schema, is_rec
  | Ptype_abstract ->
    (match type_decl.ptype_manifest with
    | Some core_type ->
      let schema, is_rec = type_of_core ~config ~recursive_types core_type in
      type_name, schema, is_rec
    | None ->
      let msg = "ppx_deriving_jsonschema: abstract type without manifest" in
      type_name, [%expr [%ocaml.error [%e estring ~loc msg]]], false)
  | Ptype_open ->
    let msg = "ppx_deriving_jsonschema: open types not supported" in
    type_name, [%expr [%ocaml.error [%e estring ~loc msg]]], false

let derive_jsonschema ~ctxt ast flag_variant_as_string flag_polymorphic_variant_tuple =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let config =
    { variant_as_string = flag_variant_as_string; polymorphic_variant_tuple = flag_polymorphic_variant_tuple }
  in
  match ast with
  (* Single type declaration *)
  | _, [ type_decl ] ->
    let type_name = type_decl.ptype_name.txt in
    let recursive_types = [ type_name ] in
    let _, schema, is_rec = derive_single_type ~loc ~config ~recursive_types type_decl in
    let schema = if is_rec then Schema.with_defs ~loc type_name schema else schema in
    [ create_value ~loc type_name schema ]
  (* Multiple type declarations (mutually recursive types) *)
  | _, type_decls when List.length type_decls > 1 ->
    (* Collect all type names in the recursive group *)
    let recursive_types = List.map (fun td -> td.ptype_name.txt) type_decls in
    let primary_type = List.hd recursive_types in
    (* Generate schemas for all types *)
    let results = List.map (derive_single_type ~loc ~config ~recursive_types) type_decls in
    let any_recursive = List.exists (fun (_, _, is_rec) -> is_rec) results in
    if any_recursive then (
      (* Generate a single value with $defs containing all types, $ref to first *)
      let defs = List.map (fun (name, schema, _) -> name, schema) results in
      let combined_schema = Schema.with_multi_defs ~loc ~primary_type defs in
      let primary_value = create_value ~loc primary_type combined_schema in
      (* Generate individual values that reference into $defs for other types *)
      let other_values =
        List.filter_map
          (fun (name, _, _) ->
            if name = primary_type then None else Some (create_value ~loc name (Schema.type_ref ~loc name)))
          results
      in
      primary_value :: other_values)
    else
      (* No recursion detected - generate independent schemas *)
      List.map (fun (name, schema, _) -> create_value ~loc name schema) results
  | _, _ -> [%str [%ocaml.error "ppx_deriving_jsonschema: unsupported type"]]

let generator () = Deriving.Generator.V2.make ~attributes (args ()) derive_jsonschema
(* let generator () = Deriving.Generator.V2.make_noarg derive_jsonschema *)

let _ : Deriving.t = Deriving.add deriver_name ~str_type_decl:(generator ())
