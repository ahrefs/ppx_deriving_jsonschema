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
let default ~loc value = annotation ~loc ("default", value)
let description ~loc description schema_expr =
  annotation ~loc ("description", [%expr `String [%e estring ~loc description]]) schema_expr

let variants ~loc ?(as_string = false) ?(compact_variants = false) constrs =
  let opt_description ~loc desc schema =
    match desc with
    | Some d -> description ~loc d schema
    | None -> schema
  in
  anyOf ~loc
    (List.map
       (function
         | `Tag (name, typs, desc) ->
           let schema =
             if as_string then const ~loc name
             else if compact_variants && typs = [] then const ~loc name
             else tuple ~loc (const ~loc name :: typs)
           in
           opt_description ~loc desc schema
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
      | [%type: string] | [%type: bytes] | [%type: string option] | [%type: bytes option] -> format ~loc fmt.txt schema
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
      | _ -> Location.raise_errorf ~loc:core_type.ptyp_loc "[@jsonschema.maximum] can only be applied to numeric types")

  let add_minimum ~loc attr core_type =
    add_schema_attr attr (fun expr schema ->
      match core_type, expr.pexp_desc with
      | [%type: int], Pexp_constant (Pconst_integer _)
      | [%type: int32], Pexp_constant (Pconst_integer _)
      | [%type: nativeint], Pexp_constant (Pconst_integer _) ->
        minimum ~loc [%expr `Int [%e expr]] schema
      | [%type: float], Pexp_constant (Pconst_float _) -> minimum ~loc [%expr `Float [%e expr]] schema
      | _ -> Location.raise_errorf ~loc:core_type.ptyp_loc "[@jsonschema.minimum] can only be applied to numeric types")

  let rec serializer_of_core_type ~loc ct =
    match ct with
    | [%type: int] | [%type: int32] | [%type: nativeint] -> [%expr fun x -> `Int x]
    | [%type: float] -> [%expr fun x -> `Float x]
    | [%type: string] | [%type: bytes] -> [%expr fun x -> `String x]
    | [%type: bool] -> [%expr fun x -> `Bool x]
    | [%type: [%t? t] option] ->
      let s = serializer_of_core_type ~loc t in
      [%expr
        fun x ->
          match x with
          | None -> `Null
          | Some v -> [%e s] v]
    | [%type: [%t? t] list] ->
      let s = serializer_of_core_type ~loc t in
      [%expr fun xs -> `List (List.map [%e s] xs)]
    | [%type: [%t? t] array] ->
      let s = serializer_of_core_type ~loc t in
      [%expr fun xs -> `List (Array.to_list (Array.map [%e s] xs))]
    | { ptyp_desc = Ptyp_tuple types; _ } ->
      let serializers = List.map (serializer_of_core_type ~loc) types in
      let vars = List.mapi (fun i _ -> Printf.sprintf "ppx_tuple_%d" i) types in
      let pats = List.map (fun v -> ppat_var ~loc { txt = v; loc }) vars in
      let exprs = List.map2 (fun s v -> [%expr [%e s] [%e evar ~loc v]]) serializers vars in
      [%expr fun [%p ppat_tuple ~loc pats] -> `List [%e elist ~loc exprs]]
    | { ptyp_desc = Ptyp_var name; _ } -> evar ~loc name
    | { ptyp_desc = Ptyp_constr (id, args); _ } ->
      let arg_serializers = List.map (serializer_of_core_type ~loc) args in
      let exp =
        type_constr_conv ~loc id ~f:(fun s -> if String.equal s "t" then "to_json" else s ^ "_to_json") arg_serializers
      in
      [%expr Ppx_deriving_jsonschema_runtime.classify [%e exp]]
    | _ ->
      Location.raise_errorf ~loc:ct.ptyp_loc
        "[@jsonschema.default] cannot serialize this type. For non-primitive types, ensure a '<type>_to_json' function \
         is in scope (e.g., add [@@deriving json] to the type definition)"

  let add_default ~loc attr core_type =
    add_schema_attr attr (fun expr schema ->
      let json_value =
        match expr.pexp_desc with
        | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> [%expr `List []]
        | Pexp_construct ({ txt = Lident "None"; _ }, None) -> [%expr `Null]
        | _ ->
          let base_type =
            match core_type with
            | [%type: [%t? t] option] -> t
            | t -> t
          in
          let serializer = serializer_of_core_type ~loc base_type in
          [%expr [%e serializer] [%e expr]]
      in
      match schema with
      | [%expr `Assoc [%e? fields]] -> [%expr `Assoc (("default", [%e json_value]) :: [%e fields])]
      | s ->
        [%expr
          match [%e s] with
          | `Assoc ppx_fields -> `Assoc (("default", [%e json_value]) :: ppx_fields)
          | ppx_other -> ppx_other])

  let add_description ~loc attr = add_schema_attr attr (fun desc schema -> description ~loc desc.txt schema)

  let add_annotations ~loc ?core_type attrs schema =
    let require_core_type field =
      match core_type with
      | Some t -> t
      | None ->
        Location.raise_errorf ~loc
          "[@jsonschema.attrs] '%s' requires a type context (use the individual [@jsonschema.%s] attribute instead)"
          field field
    in
    match attrs with
    | None -> schema
    | Some expr ->
    match expr.pexp_desc with
    | Pexp_record (fields, None) ->
      List.fold_left
        (fun schema ({ txt = label; loc = label_loc }, value) ->
          match label with
          | Lident "description" ->
            (match value.pexp_desc with
            | Pexp_constant (Pconst_string (s, _, _)) -> description ~loc s schema
            | _ ->
              Location.raise_errorf ~loc:value.pexp_loc "[@jsonschema.attrs] 'description' must be a string literal")
          | Lident "format" ->
            let ct = require_core_type "format" in
            (match value.pexp_desc with
            | Pexp_constant (Pconst_string (s, _, _)) ->
              (match ct with
              | [%type: string] | [%type: bytes] | [%type: string option] | [%type: bytes option] ->
                format ~loc s schema
              | _ ->
                Location.raise_errorf ~loc:ct.ptyp_loc
                  "[@jsonschema.attrs] 'format' can only be applied to string types")
            | _ -> Location.raise_errorf ~loc:value.pexp_loc "[@jsonschema.attrs] 'format' must be a string literal")
          | Lident "maximum" ->
            let ct = require_core_type "maximum" in
            (match ct with
            | [%type: int] | [%type: int32] | [%type: nativeint] -> maximum ~loc [%expr `Int [%e value]] schema
            | [%type: float] -> maximum ~loc [%expr `Float [%e value]] schema
            | _ ->
              Location.raise_errorf ~loc:ct.ptyp_loc
                "[@jsonschema.attrs] 'maximum' can only be applied to numeric types")
          | Lident "minimum" ->
            let ct = require_core_type "minimum" in
            (match ct with
            | [%type: int] | [%type: int32] | [%type: nativeint] -> minimum ~loc [%expr `Int [%e value]] schema
            | [%type: float] -> minimum ~loc [%expr `Float [%e value]] schema
            | _ ->
              Location.raise_errorf ~loc:ct.ptyp_loc
                "[@jsonschema.attrs] 'minimum' can only be applied to numeric types")
          | Lident name -> Location.raise_errorf ~loc:label_loc "[@jsonschema.attrs] unknown field: '%s'" name
          | _ -> Location.raise_errorf ~loc:label_loc "[@jsonschema.attrs] expected a simple field name")
        schema fields
    | _ ->
      Location.raise_errorf ~loc:expr.pexp_loc "[@jsonschema.attrs] expects a record expression: { field = value; ... }"
end
