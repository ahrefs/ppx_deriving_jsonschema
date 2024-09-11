open Ppxlib
open Ast_builder.Default

let deriver_name = "jsonschema"

let default_attribute =
  Attribute.declare "ppx_deriving_yojson.of_yojson.default" Attribute.Context.label_declaration
    Ast_pattern.(single_expr_payload __)
    (fun expr -> expr)

let attributes = [ Attribute.T default_attribute ]

let args () = Deriving.Args.(empty)
(* let args () = Deriving.Args.(empty +> arg "option1" (eint __) +> flag "flag") *)

let deps = []

let predefined_types = [ "string"; "int"; "float"; "bool" ]
let is_predefined_type type_name = List.mem type_name predefined_types

let type_ref ~loc type_name =
  let name = estring ~loc ("#/definitions/" ^ type_name) in
  [%expr `Assoc [ "$ref", `String [%e name] ]]

let type_def ~loc type_name =
  let type_name =
    match type_name with
    | "int" -> "integer"
    | "float" -> "number"
    | "bool" -> "boolean"
    | _ -> type_name
  in
  [%expr `Assoc [ "type", `String [%e estring ~loc type_name] ]]

let enum ~loc values =
  let values = List.map (fun name -> [%expr `String [%e estring ~loc name]]) values in
  [%expr `Assoc [ "type", `String "string"; "enum", `List [%e elist ~loc values] ]]

let array_ ~loc element_type = [%expr `Assoc [ "type", `String "array"; "items", [%e element_type] ]]

let tuple ~loc elements = [%expr `Assoc [ "type", `String "array"; "items", `List [%e elist ~loc elements] ]]

let value_name_pattern ~loc type_name = [%pat? [%p ppat_var ~loc { txt = type_name ^ "_jsonschema"; loc }]]

let create_value ~loc name value = [%stri let[@warning "-32"] [%p value_name_pattern ~loc name] = [%e value]]

let is_optional_type core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr ({ txt = Lident "option"; _ }, _) -> true
  | _ -> false

let rec type_of_core ~loc core_type =
  match core_type.ptyp_desc with
  | Ptyp_constr ({ txt = Lident ("list" | "array"); _ }, [ t ]) ->
    let t = type_of_core ~loc t in
    array_ ~loc t
  | Ptyp_constr ({ txt = Lident "option"; _ }, [ t ]) ->
    (* might have to tweak to allow nullable? *)
    type_of_core ~loc t
  | Ptyp_constr ({ txt = Lident type_name; _ }, []) ->
    if is_predefined_type type_name then type_def ~loc type_name else type_ref ~loc type_name
  | Ptyp_tuple types ->
    let ts = List.map (type_of_core ~loc) types in
    tuple ~loc ts
  | _ ->
    Format.printf "unsuported core type: %a\n------\n" Astlib.Pprintast.core_type core_type;
    (* todo:
       - types living in different modules
       - types with parameters
    *)
    [%expr (* This type is unknown, placeholder to accept anything *) `Assoc []]

(* todo: add option to inline types instead of using definitions references *)
let object_ ~loc fields =
  let fields, required =
    List.fold_left
      (fun (fields, required) { pld_name = { txt = name; _ }; pld_type; _ } ->
        let type_def = type_of_core ~loc pld_type in
        ( [%expr [%e estring ~loc name], [%e type_def]] :: fields,
          if is_optional_type pld_type then required else name :: required ))
      ([], []) fields
  in
  [%expr
    `Assoc
      [
        "type", `String "object";
        "properties", `Assoc [%e elist ~loc fields];
        "required", `List [%e elist ~loc (List.map (estring ~loc) required)];
      ]]

let derive_jsonschema ~ctxt ast =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match ast with
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_variant variants; _ } ] ->
    let variants =
      List.filter
        (fun { pcd_args; _ } ->
          match pcd_args with
          | Pcstr_record _ | Pcstr_tuple (_ :: _) ->
            (* todo: emit an error when a type can't be turned into a valid json schema *)
            Format.printf "unsuported variant constructor with a payload: %a\n======\n"
              Format.(pp_print_list Astlib.Pprintast.type_declaration)
              (snd ast);
            false
          | Pcstr_tuple [] -> true)
        variants
    in
    let names = List.map (fun { pcd_name = { txt = value; _ }; _ } -> value) variants in
    let jsonschema_expr = create_value ~loc type_name (enum ~loc names) in
    [ jsonschema_expr ]
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_record label_declarations; _ } ] ->
    let jsonschema_expr = create_value ~loc type_name (object_ ~loc label_declarations) in
    [ jsonschema_expr ]
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_abstract; ptype_manifest = Some core_type; _ } ] ->
    let jsonschema_expr = create_value ~loc type_name (type_of_core ~loc core_type) in
    [ jsonschema_expr ]
  | _, ast ->
    Format.printf "unsuported type: %a\n======\n" Format.(pp_print_list Astlib.Pprintast.type_declaration) ast;
    [%str [%ocaml.error "Oops, jsonschema deriving does not support this type"]]

let generator () = Deriving.Generator.V2.make (args ()) derive_jsonschema
(* let generator () = Deriving.Generator.V2.make_noarg derive_jsonschema *)

let _ : Deriving.t = Deriving.add deriver_name ~str_type_decl:(generator ())
