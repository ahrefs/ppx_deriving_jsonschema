open Ppxlib
open Ast_builder.Default

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

let nullable ~loc schema =
  match schema with
  | [%expr `Assoc [ "type", `String [%e? t] ]] -> [%expr `Assoc [ "type", `List [ `String [%e t]; `String "null" ] ]]
  | s -> [%expr `Assoc [ "anyOf", `List [ [%e s]; `Assoc [ "type", `String "null" ] ] ]]

let description ~loc ~description:d schema_expr =
  [%expr
    match [%e schema_expr] with
    | `Assoc fields -> `Assoc (("description", `String [%e estring ~loc d]) :: fields)
    | s -> s]

let variant_branch ~loc ~description_opt name typs ~as_string =
  let branch = if as_string then const ~loc name else tuple ~loc (const ~loc name :: typs) in
  match description_opt with
  | Some d -> description ~loc ~description:d branch
  | None -> branch

let variant_as_string ~loc constrs =
  anyOf ~loc
    (List.map
       (function
         | `Tag (name, _typs, desc) -> variant_branch ~loc ~description_opt:desc name [] ~as_string:true
         | `Inherit typ -> typ)
       constrs)

let variant_as_array ~loc constrs =
  anyOf ~loc
    (List.map
       (function
         | `Tag (name, typs, desc) -> variant_branch ~loc ~description_opt:desc name typs ~as_string:false
         | `Inherit typ -> typ)
       constrs)
