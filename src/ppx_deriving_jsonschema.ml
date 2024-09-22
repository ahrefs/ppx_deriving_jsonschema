open Ppxlib
open Ast_builder.Default

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

let args () = Deriving.Args.(empty)
(* let args () = Deriving.Args.(empty +> arg "option1" (eint __) +> flag "flag") *)

let deps = []

let predefined_types = [ "string"; "int"; "float"; "bool" ]
let is_predefined_type type_name = List.mem type_name predefined_types

let type_ref ~loc type_name =
  let name = estring ~loc ("#/$defs/" ^ type_name) in
  [%expr `Assoc [ "$ref", `String [%e name] ]]

let type_def ~loc type_name = [%expr `Assoc [ "type", `String [%e estring ~loc type_name] ]]

let null ~loc = [%expr `Assoc [ "type", `String "null" ]]

let char ~loc = [%expr `Assoc [ "type", `String "string"; "minLength", `Int 1; "maxLength", `Int 1 ]]

let enum ~loc values =
  let values = List.map (fun name -> [%expr `String [%e estring ~loc name]]) values in
  [%expr `Assoc [ "type", `String "string"; "enum", `List [%e elist ~loc values] ]]

let array_ ~loc element_type = [%expr `Assoc [ "type", `String "array"; "items", [%e element_type] ]]

let tuple ~loc elements = [%expr `Assoc [ "type", `String "array"; "items", `List [%e elist ~loc elements] ]]

let value_name_pattern ~loc type_name = ppat_var ~loc { txt = type_name ^ "_jsonschema"; loc }

let create_value ~loc name value =
  [%stri let[@warning "-32-39"] (* rec *) [%p value_name_pattern ~loc name (* : [< `Assoc of _ list ] *)] = [%e value]]

let is_optional_type core_type =
  match core_type with
  | [%type: [%t? _] option] -> true
  | _ -> false

let rec type_of_core ~loc core_type =
  match core_type with
  | [%type: int] | [%type: int32] | [%type: int64] | [%type: nativeint] -> type_def ~loc "integer"
  | [%type: float] -> type_def ~loc "number"
  | [%type: string] | [%type: bytes] -> type_def ~loc "string"
  | [%type: bool] -> type_def ~loc "boolean"
  | [%type: char] -> char ~loc
  | [%type: unit] -> null ~loc
  | [%type: [%t? t] option] -> type_of_core ~loc t
  | [%type: [%t? t] ref] -> type_of_core ~loc t
  | [%type: [%t? t] list] | [%type: [%t? t] array] ->
    let t = type_of_core ~loc t in
    array_ ~loc t
  | _ ->
  match core_type.ptyp_desc with
  | Ptyp_constr (id, []) ->
    (* todo: support using references with [type_ref ~loc type_name] instead of inlining everything *)
    type_constr_conv ~loc id ~f:(fun s -> s ^ "_jsonschema") []
  | Ptyp_tuple types ->
    let ts = List.map (type_of_core ~loc) types in
    tuple ~loc ts
  | Ptyp_variant (row_fields, _, _) ->
    let constr_names =
      List.map
        (fun row_field ->
          let name_overwrite = Attribute.get jsonschema_polymorphic_variant_name row_field in
          match name_overwrite with
          | Some name -> name
          | None ->
          match row_field with
          | { prf_desc = Rtag (name, _, _); _ } -> name.txt
          | { prf_desc = Rinherit _core_type; _ } ->
            Format.asprintf "unsupported polymorphic variant type: %a" Astlib.Pprintast.core_type core_type (* todo: *))
        row_fields
    in
    enum ~loc constr_names
  | _ ->
    (* Format.printf "unsuported core type: %a\n------\n" Astlib.Pprintast.core_type core_type; *)
    [%expr
      (* todo: this type is unknown, placeholder to accept anything. Should create an error instead. *)
      `Assoc
        [
          "unsuported core type", `String [%e estring ~loc (Format.asprintf "%a" Astlib.Pprintast.core_type core_type)];
        ]]

(* todo: add option to inline types instead of using definitions references *)
let object_ ~loc fields =
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
          | Some def -> type_ref ~loc def
          | None -> type_of_core ~loc pld_type
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

let derive_jsonschema ~ctxt ast =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match ast with
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_variant variants; _ } ] ->
    let variants =
      List.map
        (fun ({ pcd_args; pcd_name = { txt = name; _ }; _ } as var) ->
          let name_overwrite = Attribute.get jsonschema_variant_name var in
          match name_overwrite with
          | Some name -> name
          | None ->
          match pcd_args with
          | Pcstr_record _ | Pcstr_tuple (_ :: _) ->
            (* todo: emit an error when a type can't be turned into a valid json schema *)
            Format.asprintf "unsuported variant constructor with a payload: %a"
              Format.(pp_print_list Astlib.Pprintast.type_declaration)
              (snd ast)
          | Pcstr_tuple [] -> name)
        variants
    in
    (* let names = List.map (fun { pcd_name = { txt = value; _ }; _ } -> value) variants in *)
    let jsonschema_expr = create_value ~loc type_name (enum ~loc variants) in
    [ jsonschema_expr ]
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_record label_declarations; _ } ] ->
    let jsonschema_expr = create_value ~loc type_name (object_ ~loc label_declarations) in
    [ jsonschema_expr ]
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_abstract; ptype_manifest = Some core_type; _ } ] ->
    let jsonschema_expr = create_value ~loc type_name (type_of_core ~loc core_type) in
    [ jsonschema_expr ]
  | _, _ast ->
    (* Format.printf "unsuported type: %a\n======\n" Format.(pp_print_list Astlib.Pprintast.type_declaration) ast; *)
    [%str [%ocaml.error "Oops, jsonschema deriving does not support this type"]]

let generator () = Deriving.Generator.V2.make ~attributes (args ()) derive_jsonschema
(* let generator () = Deriving.Generator.V2.make_noarg derive_jsonschema *)

let _ : Deriving.t = Deriving.add deriver_name ~str_type_decl:(generator ())
