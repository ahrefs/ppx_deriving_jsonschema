open Ppxlib
open Ast_builder.Default

type config = {
  variant_as_array : bool;
    (** Encode variants as arrays of string enum instead of a string enum.
      Provides compatibility with the encoding used by the [ppx_deriving_json]
      and [ppx_yojson_conv] extensions. *)
}

let deriver_name = "jsonschema"

let jsonschema_key =
  Attribute.declare "jsonschema.key" Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun x -> x)

let jsonschema_ref =
  Attribute.declare "jsonschema.ref" Attribute.Context.label_declaration
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun x -> x)

let jsonschema_variant_name =
  Attribute.declare "jsonschema.name" Attribute.Context.constructor_declaration
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun x -> x)

let jsonschema_polymorphic_variant_name =
  Attribute.declare "jsonschema.name" Attribute.Context.rtag
    Ast_pattern.(pstr (pstr_eval (estring __) nil ^:: nil))
    (fun x -> x)

let attributes =
  [
    Attribute.T jsonschema_key;
    Attribute.T jsonschema_ref;
    Attribute.T jsonschema_variant_name;
    Attribute.T jsonschema_polymorphic_variant_name;
  ]

(* let args () = Deriving.Args.(empty) *)
let args () = Deriving.Args.(empty +> flag "variant_as_array")

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
         match config.variant_as_array with
         | true -> Schema.tuple ~loc (Schema.const ~loc name :: typs)
         | false -> Schema.const ~loc name)
       constrs)

let value_name_pattern ~loc type_name = ppat_var ~loc { txt = type_name ^ "_jsonschema"; loc }

let create_value ~loc name value =
  [%stri let[@warning "-32-39"] (* rec *) [%p value_name_pattern ~loc name (* : [< `Assoc of _ list ] *)] = [%e value]]

let is_optional_type core_type =
  match core_type with
  | [%type: [%t? _] option] -> true
  | _ -> false

let rec type_of_core ~loc ~config core_type =
  match core_type with
  | [%type: int] | [%type: int32] | [%type: int64] | [%type: nativeint] -> Schema.type_def ~loc "integer"
  | [%type: float] -> Schema.type_def ~loc "number"
  | [%type: string] | [%type: bytes] -> Schema.type_def ~loc "string"
  | [%type: bool] -> Schema.type_def ~loc "boolean"
  | [%type: char] -> Schema.char ~loc
  | [%type: unit] -> Schema.null ~loc
  | [%type: [%t? t] option] -> type_of_core ~loc ~config t
  | [%type: [%t? t] ref] -> type_of_core ~loc ~config t
  | [%type: [%t? t] list] | [%type: [%t? t] array] ->
    let t = type_of_core ~loc ~config t in
    Schema.array_ ~loc t
  | _ ->
  match core_type.ptyp_desc with
  | Ptyp_constr (id, []) ->
    (* todo: support using references with [type_ref ~loc type_name] instead of inlining everything *)
    type_constr_conv ~loc id ~f:(fun s -> s ^ "_jsonschema") []
  | Ptyp_tuple types ->
    let ts = List.map (type_of_core ~loc ~config) types in
    Schema.tuple ~loc ts
  | Ptyp_variant (row_fields, _, _) ->
    let constrs =
      List.map
        (fun row_field ->
          match row_field with
          | { prf_desc = Rtag (name, _, typs); _ } ->
            let name =
              match Attribute.get jsonschema_polymorphic_variant_name row_field with
              | Some name -> name
              | None -> name.txt
            in
            let typs = List.map (type_of_core ~loc ~config) typs in
            `Tag (name, typs)
          | { prf_desc = Rinherit core_type; _ } ->
            let typ = type_of_core ~loc ~config core_type in
            `Inherit typ)
        row_fields
    in
    (* todo: raise an error if encoding is as string and constructor has a payload *)
    let v =
      match config.variant_as_array with
      | true -> variant_as_array ~loc constrs
      | false -> variant_as_string ~loc constrs
    in
    v
  | _ ->
    (* Format.printf "unsuported core type: %a\n------\n" Astlib.Pprintast.core_type core_type; *)
    [%expr
      (* todo: this type is unknown, placeholder to accept anything. Should create an error instead. *)
      `Assoc
        [
          "unsuported core type", `String [%e estring ~loc (Format.asprintf "%a" Astlib.Pprintast.core_type core_type)];
        ]]

(* todo: add option to inline types instead of using definitions references *)
let object_ ~loc ~config fields =
  let fields, required =
    List.fold_left
      (fun (fields, required) ({ pld_name = { txt = name; _ }; pld_type; _ } as field) ->
        let name =
          match Attribute.get jsonschema_key field with
          | Some name -> name
          | None -> name
        in
        let type_def =
          match Attribute.get jsonschema_ref field with
          | Some def -> Schema.type_ref ~loc def
          | None -> type_of_core ~loc ~config pld_type
        in
        ( [%expr [%e estring ~loc name], [%e type_def]] :: fields,
          if is_optional_type pld_type then required else name :: required ))
      ([], []) fields
  in
  let required = List.map (fun name -> [%expr `String [%e estring ~loc name]]) required in
  [%expr
    `Assoc
      [
        "type", `String "object";
        "properties", `Assoc [%e elist ~loc fields];
        "required", `List [%e elist ~loc required];
      ]]

let derive_jsonschema ~ctxt ast flag_variant_as_array =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let config = { variant_as_array = flag_variant_as_array } in
  match ast with
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_variant variants; _ } ] ->
    let variants =
      List.map
        (fun ({ pcd_args; pcd_name = { txt = name; _ }; _ } as var) ->
          let name =
            match Attribute.get jsonschema_variant_name var with
            | Some name -> name
            | None -> name
          in
          match pcd_args with
          | Pcstr_record label_declarations ->
            let typs = [ object_ ~loc ~config label_declarations ] in
            `Tag (name, typs)
          | Pcstr_tuple typs ->
            let types = List.map (type_of_core ~loc ~config) typs in
            `Tag (name, types))
        variants
    in
    let v =
      (* todo: raise an error if encoding is as string and constructor has a payload *)
      match config.variant_as_array with
      | true -> variant_as_array ~loc variants
      | false -> variant_as_string ~loc variants
    in
    let jsonschema_expr = create_value ~loc type_name v in
    [ jsonschema_expr ]
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_record label_declarations; _ } ] ->
    let jsonschema_expr = create_value ~loc type_name (object_ ~loc ~config label_declarations) in
    [ jsonschema_expr ]
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_abstract; ptype_manifest = Some core_type; _ } ] ->
    let jsonschema_expr = create_value ~loc type_name (type_of_core ~loc ~config core_type) in
    [ jsonschema_expr ]
  | _, _ast ->
    (* Format.printf "unsuported type: %a\n======\n" Format.(pp_print_list Astlib.Pprintast.type_declaration) ast; *)
    [%str [%ocaml.error "Oops, jsonschema deriving does not support this type"]]

let generator () = Deriving.Generator.V2.make ~attributes (args ()) derive_jsonschema
(* let generator () = Deriving.Generator.V2.make_noarg derive_jsonschema *)

let _ : Deriving.t = Deriving.add deriver_name ~str_type_decl:(generator ())
