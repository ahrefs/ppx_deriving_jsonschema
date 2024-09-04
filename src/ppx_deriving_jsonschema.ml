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

let enum ~loc values =
  let values = List.map (fun name -> [%expr `String [%e estring ~loc name]]) values in
  [%expr `Assoc [ "type", `String "string"; "enum", `List [%e elist ~loc values] ]]

let derive_jsonschema ~ctxt ast =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match ast with
  | _, [ { ptype_name = { txt = type_name; _ }; ptype_kind = Ptype_variant variants; _ } ] ->
    let _expr_string = Ast_builder.Default.estring ~loc in
    let function_name_pattern = [%pat? [%p ppat_var ~loc { txt = type_name ^ "_jsonschema"; loc }]] in
    let names = List.map (fun { pcd_name = { txt = value; _ }; _ } -> value) variants in
    let jsonschema_expr = [%stri let[@warning "-32"] [%p function_name_pattern] = [%e enum ~loc names]] in
    (* Uncomment to see the generated code *)
    (* print_endline (Astlib.Pprintast.string_of_structure [ jsonschema_expr ]); *)
    [ jsonschema_expr ]
  | _ -> [%str [%ocaml.error "Ops, jsonschema deriving must be applied to a variant type without args"]]

(* return "deriving jsonschem" *)
(* if flag then return "flag is on"
   else (
     match option1 with
     | Some i -> return (Printf.sprintf "option is %d" i)
     | None -> return "flag and option are not set") *)

let generator () = Deriving.Generator.V2.make (args ()) derive_jsonschema
(* let generator () = Deriving.Generator.V2.make_noarg derive_jsonschema *)

let _ : Deriving.t = Deriving.add deriver_name ~str_type_decl:(generator ())
