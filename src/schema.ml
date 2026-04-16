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

let annotation ~loc (name, value) schema =
  match schema with
  | [%expr `Assoc [%e? fields]] -> [%expr `Assoc (([%e estring ~loc name], [%e value]) :: [%e fields])]
  | s -> s

let format ~loc format = annotation ~loc ("format", [%expr `String [%e estring ~loc format]])
let maximum ~loc maximum = annotation ~loc ("maximum", maximum)
let minimum ~loc minimum = annotation ~loc ("minimum", minimum)
let description ~loc description schema_expr =
  annotation ~loc ("description", [%expr `String [%e estring ~loc description]]) schema_expr

let variants ~loc ?(as_string = false) constrs =
  let opt_description ~loc desc schema =
    match desc with
    | Some d -> description ~loc d schema
    | None -> schema
  in
  anyOf ~loc
    (List.map
       (function
         | `Tag (name, typs, desc) ->
           opt_description ~loc desc (if as_string then const ~loc name else tuple ~loc (const ~loc name :: typs))
         | `Inherit typ -> typ)
       constrs)

module Annotation = struct
  let add_schema_attr (attr, node) f schema =
    match Attribute.get attr node with
    | Some v -> f v schema
    | None -> schema

  let add_format ~loc attr core_type =
    add_schema_attr attr (fun fmt schema ->
        match core_type with
        | [%type: string] | [%type: bytes] | [%type: string option] | [%type: bytes option] ->
          format ~loc fmt.txt schema
        | _ ->
          Location.raise_errorf ~loc:core_type.ptyp_loc
            "[@jsonschema.format] can only be applied to string or bytes types")

  let add_maximum ~loc attr core_type =
    add_schema_attr attr (fun expr schema ->
        match core_type, expr.pexp_desc with
        | [%type: int], Pexp_constant (Pconst_integer _)
        | [%type: int32], Pexp_constant (Pconst_integer _)
        | [%type: nativeint], Pexp_constant (Pconst_integer _) ->
          maximum ~loc [%expr `Int [%e expr]] schema
        | [%type: float], Pexp_constant (Pconst_float _) -> maximum ~loc [%expr `Float [%e expr]] schema
        | _ ->
          Location.raise_errorf ~loc:core_type.ptyp_loc "[@jsonschema.maximum] can only be applied to numeric types")

  let add_minimum ~loc attr core_type =
    add_schema_attr attr (fun expr schema ->
        match core_type, expr.pexp_desc with
        | [%type: int], Pexp_constant (Pconst_integer _)
        | [%type: int32], Pexp_constant (Pconst_integer _)
        | [%type: nativeint], Pexp_constant (Pconst_integer _) ->
          minimum ~loc [%expr `Int [%e expr]] schema
        | [%type: float], Pexp_constant (Pconst_float _) -> minimum ~loc [%expr `Float [%e expr]] schema
        | _ ->
          Location.raise_errorf ~loc:core_type.ptyp_loc "[@jsonschema.minimum] can only be applied to numeric types")

  let add_description ~loc attr = add_schema_attr attr (fun desc schema -> description ~loc desc.txt schema)

end
