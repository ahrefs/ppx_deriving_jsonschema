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

let type_def ~loc type_name = [%expr `Assoc [ "type", `String [%e estring ~loc type_name] ]]

let enum ~loc values =
  let values = List.map (fun name -> [%expr `String [%e estring ~loc name]]) values in
  [%expr `Assoc [ "type", `String "string"; "enum", `List [%e elist ~loc values] ]]

(* todo: add option to inline types instead of using definitions references *)
let object_ ~loc fields =
  let fields =
    List.map
      (fun { pld_name = { txt = name; _ }; pld_type; _ } ->
        let type_def =
          match pld_type.ptyp_desc with
          | Ptyp_constr ({ txt = Lident name; _ }, _) when is_predefined_type name -> type_def ~loc name
          | Ptyp_constr ({ txt = Lident name; _ }, _) -> type_ref ~loc name
          | _ -> [%expr (* This type is unknown, placeholder to accept anything *) `Assoc []]
        in
        [%expr [%e estring ~loc name], [%e type_def]])
      fields
  in
  [%expr `Assoc [ "type", `String "object"; "properties", `Assoc [%e elist ~loc fields] ]]

let array_ ~loc element_type = [%expr `Assoc [ "type", `String "array"; "items", [%e element_type] ]]

let value_name_pattern ~loc type_name = [%pat? [%p ppat_var ~loc { txt = type_name ^ "_jsonschema"; loc }]]

let create_value ~loc name value = [%stri let[@warning "-32"] [%p value_name_pattern ~loc name] = [%e value]]

let derive_jsonschema ~ctxt ast =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match ast with
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_variant variants; _ } ] ->
    let names = List.map (fun { pcd_name = { txt = value; _ }; _ } -> value) variants in
    let jsonschema_expr = create_value ~loc type_name (enum ~loc names) in
    [ jsonschema_expr ]
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_record label_declarations; _ } ] ->
    let jsonschema_expr = create_value ~loc type_name (object_ ~loc label_declarations) in
    [ jsonschema_expr ]
  | ( _,
      [
        {
          ptype_name = { txt = type_name; _ };
          ptype_kind = Ptype_abstract;
          ptype_manifest =
            Some
              {
                ptyp_desc =
                  Ptyp_constr
                    ( { txt = Lident ("list" | "array"); _ },
                      [
                        (* should have some kind of function to recursively turn Ptyp_constr into a type definition or reference *)
                        { ptyp_desc = Ptyp_constr ({ txt = Lident array_type_name; _ }, []); _ };
                      ] );
                _;
              };
          _;
        };
      ] ) ->
    let typ =
      if is_predefined_type array_type_name then type_def ~loc array_type_name else type_ref ~loc array_type_name
    in
    let jsonschema_expr = create_value ~loc type_name (array_ ~loc typ) in
    [ jsonschema_expr ]
  | _, ast ->
    Format.printf "unsuported type: %a\n======\n" Format.(pp_print_list Astlib.Pprintast.type_declaration) ast;
    [%str [%ocaml.error "Ops, jsonschema deriving must be applied to a variant type without args"]]

(* return "deriving jsonschem" *)
(* if flag then return "flag is on"
   else (
     match option1 with
     | Some i -> return (Printf.sprintf "option is %d" i)
     | None -> return "flag and option are not set") *)

let generator () = Deriving.Generator.V2.make (args ()) derive_jsonschema
(* let generator () = Deriving.Generator.V2.make_noarg derive_jsonschema *)

let _ : Deriving.t = Deriving.add deriver_name ~str_type_decl:(generator ())
