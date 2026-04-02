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

let jsonschema_option = Attribute.declare_flag "jsonschema.option" Attribute.Context.label_declaration

let attributes =
  [
    Attribute.T jsonschema_key;
    Attribute.T jsonschema_ref;
    Attribute.T jsonschema_variant_name;
    Attribute.T jsonschema_polymorphic_variant_name;
    Attribute.T jsonschema_td_allow_extra_fields;
    Attribute.T jsonschema_cd_allow_extra_fields;
    Attribute.T jsonschema_option;
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

  (* Make a schema explicitly nullable.
     For simple {"type": "X"} schemas, produces {"type": ["X", "null"]}.
     For complex schemas, falls back to {"anyOf": [schema, {"type": "null"}]}. *)
  let nullable ~loc schema =
    [%expr
      match [%e schema] with
      | `Assoc [ ("type", `String t) ] -> `Assoc [ "type", `List [ `String t; `String "null" ] ]
      | s -> `Assoc [ "anyOf", `List [ s; `Assoc [ "type", `String "null" ] ] ]]

  let with_defs ~loc type_name ~extra_defs_var schema =
    [%expr
      let _ppx_body = [%e schema] in
      `Assoc
        [
          "$id", `String [%e estring ~loc ("urn:jsonschema:" ^ type_name)];
          "$defs", `Assoc (([%e estring ~loc type_name], _ppx_body) :: ![%e extra_defs_var]);
          "$ref", `String [%e estring ~loc ("#/$defs/" ^ type_name)];
        ]]

  (* For mutually recursive types: multiple definitions, ref to first type.
     Schemas are bound to _ppx_body_N variables so they execute before !extra_defs_var
     is read — this ensures any hoisted $defs accumulate into extra_defs_var first. *)
  let with_multi_defs ~loc ~primary_type ~extra_defs_var defs =
    let id = "urn:jsonschema:" ^ primary_type in
    let indexed = List.mapi (fun i (name, schema) -> i, name, schema) defs in
    let body_vars = List.map (fun (i, name, _) -> name, Printf.sprintf "_ppx_body_%d" i) indexed in
    let pairs_expr =
      elist ~loc (List.map (fun (name, vname) -> [%expr [%e estring ~loc name], [%e evar ~loc vname]]) body_vars)
    in
    let base_expr =
      [%expr
        `Assoc
          [
            "$id", `String [%e estring ~loc id];
            "$defs", `Assoc ([%e pairs_expr] @ ![%e extra_defs_var]);
            "$ref", `String [%e estring ~loc ("#/$defs/" ^ primary_type)];
          ]]
    in
    List.fold_right
      (fun (i, _name, schema) acc ->
        let vname = Printf.sprintf "_ppx_body_%d" i in
        [%expr
          let [%p ppat_var ~loc { txt = vname; loc }] = [%e schema] in
          [%e acc]])
      indexed base_expr
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

let create_value ~loc name value = [%stri let[@warning "-32-39"] [%p value_name_pattern ~loc name] = [%e value]]

(* Returns (schema_expression, is_recursive)
   recursive_types: list of type names in a mutually recursive group
   extra_defs_var: when Some edv, edv is an expression for a (string * t) list ref that
     accumulates hoisted $defs from parametric recursive sub-schemas *)
let rec type_of_core ~config ?(recursive_types = []) ?extra_defs_var core_type =
  let loc = core_type.ptyp_loc in
  match core_type with
  | [%type: int] | [%type: int32] | [%type: int64] | [%type: nativeint] -> Schema.type_def ~loc "integer", false
  | [%type: float] -> Schema.type_def ~loc "number", false
  | [%type: string] | [%type: bytes] -> Schema.type_def ~loc "string", false
  | [%type: bool] -> Schema.type_def ~loc "boolean", false
  | [%type: char] -> Schema.char ~loc, false
  | [%type: unit] -> Schema.null ~loc, false
  | [%type: [%t? t] option] ->
    let s, is_rec = type_of_core ~config ~recursive_types ?extra_defs_var t in
    Schema.nullable ~loc s, is_rec
  | [%type: [%t? t] ref] -> type_of_core ~config ~recursive_types ?extra_defs_var t
  | [%type: [%t? t] list] | [%type: [%t? t] array] ->
    let t, is_rec = type_of_core ~config ~recursive_types ?extra_defs_var t in
    Schema.array_ ~loc t, is_rec
  | _ ->
  match core_type.ptyp_desc with
  | Ptyp_var name -> evar ~loc name, false
  | Ptyp_constr (id, args) ->
    (match id.txt with
    | Lident name when List.mem name recursive_types ->
      (* Recursive reference: emit $ref regardless of type arguments.
         The $defs entry for this type is the canonical definition; inlining
         its schema here (with concrete args) would produce the wrong schema
         and lose the self-reference structure. *)
      Schema.type_ref ~loc name, true
    | _ ->
      let results = List.map (type_of_core ~config ~recursive_types ?extra_defs_var) args in
      let args = List.map fst results in
      let is_rec = List.exists snd results in
      let schema = type_constr_conv ~loc id ~f:(fun s -> s ^ "_jsonschema") args in
      (match is_rec, extra_defs_var with
      | true, Some edv ->
        (* A recursive $ref was passed as an arg to a parametric type.
           The parametric type's schema will have {$id, $defs, $ref}; the $id
           creates a JSON Schema resource boundary so internal $refs like
           "#/$defs/outer" would resolve against the inner $id — wrong.
           Fix: hoist the inner $defs into the outer type's _ppx_eds accumulator
           with unique suffixed names, and return just a renamed $ref. *)
        let suffix = estring ~loc (Printf.sprintf "%d_%d" loc.loc_start.pos_lnum loc.loc_start.pos_cnum) in
        ( [%expr
            let _ppx_sub = [%e schema] in
            match _ppx_sub with
            | `Assoc _ppx_pairs when List.mem_assoc "$defs" _ppx_pairs ->
              let _ppx_inner_defs =
                match List.assoc_opt "$defs" _ppx_pairs with
                | Some (`Assoc d) -> d
                | _ -> []
              in
              let _ppx_suffix = [%e suffix] in
              let _ppx_mk n = n ^ "__" ^ _ppx_suffix in
              let _ppx_rmap = List.map (fun (n, _) -> n, _ppx_mk n) _ppx_inner_defs in
              let rec _ppx_ren t =
                match t with
                | `Assoc pairs ->
                  `Assoc
                    (List.map
                       (fun (k, v) ->
                         let v' =
                           if k = "$ref" then (
                             match v with
                             | `String r when String.length r > 8 && String.sub r 0 8 = "#/$defs/" ->
                               let n = String.sub r 8 (String.length r - 8) in
                               `String
                                 ("#/$defs/"
                                 ^
                                 match List.assoc_opt n _ppx_rmap with
                                 | Some m -> m
                                 | None -> n)
                             | _ -> v)
                           else _ppx_ren v
                         in
                         k, v')
                       pairs)
                | `List xs -> `List (List.map _ppx_ren xs)
                | other -> other
              in
              let _ppx_hoisted = List.map (fun (n, b) -> _ppx_mk n, _ppx_ren b) _ppx_inner_defs in
              [%e edv] := ![%e edv] @ _ppx_hoisted;
              (match List.assoc_opt "$ref" _ppx_pairs with
              | Some (`String _ppx_r) when String.length _ppx_r > 8 && String.sub _ppx_r 0 8 = "#/$defs/" ->
                let _ppx_n = String.sub _ppx_r 8 (String.length _ppx_r - 8) in
                `Assoc [ "$ref", `String ("#/$defs/" ^ _ppx_mk _ppx_n) ]
              | _ -> `Assoc (List.filter (fun (k, _) -> k <> "$id") _ppx_pairs))
            | _ppx_other -> _ppx_other],
          true )
      | _ ->
        (* Non-recursive arg (or no accumulator): add a location-based $id to mark
           the resource boundary so that any internal $defs refs resolve correctly. *)
        let unique_id =
          estring ~loc
            (Printf.sprintf "urn:jsonschema:%s:%d:%d" loc.loc_start.pos_fname loc.loc_start.pos_lnum
               loc.loc_start.pos_cnum)
        in
        ( [%expr
            match [%e schema] with
            | `Assoc pairs when List.mem_assoc "$defs" pairs ->
              `Assoc (("$id", `String [%e unique_id]) :: List.filter (fun (k, _) -> k <> "$id") pairs)
            | other -> other],
          is_rec )))
  | Ptyp_tuple types ->
    let results = List.map (type_of_core ~config ~recursive_types ?extra_defs_var) types in
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
            let results = List.map (type_of_core ~config ~recursive_types ?extra_defs_var) raw_typs in
            let typs = List.map fst results in
            let typs_rec = List.exists snd results in
            `Tag (name, typs) :: constrs, is_rec || typs_rec
          | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
            Location.raise_errorf ~loc "ppx_deriving_jsonschema: polymorphic_variant/Rtag/&"
          | Rinherit core_type ->
            let typ, typ_rec = type_of_core ~config ~recursive_types ?extra_defs_var core_type in
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
let object_ ~loc ~config ?(recursive_types = []) ?extra_defs_var fields allow_extra_fields =
  let fields, required, is_rec =
    List.fold_left
      (fun (fields, required, is_rec) ({ pld_name; pld_type; pld_loc = _loc; _ } as field) ->
        let name =
          match Attribute.get jsonschema_key field with
          | Some name -> name.txt
          | None -> pld_name.txt
        in
        let drop_required = Attribute.has_flag jsonschema_option field in
        let type_def, field_rec =
          match Attribute.get jsonschema_ref field with
          | Some def -> Schema.type_ref ~loc def.txt, false
          | None ->
          match pld_type with
          | [%type: [%t? inner] option] ->
            let s, r = type_of_core ~config ~recursive_types ?extra_defs_var inner in
            Schema.nullable ~loc s, r
          | _ -> type_of_core ~config ~recursive_types ?extra_defs_var pld_type
        in
        ( [%expr [%e estring ~loc name], [%e type_def]] :: fields,
          (if drop_required then required else { txt = name; loc } :: required),
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

(* Wraps [body] in nested lambdas, one per type parameter.
   Parametric types like [('a, 'b) t] derive as [fun a b -> <schema>],
   so callers can pass schemas for each type variable.
   Use [~prefix:"_"] to mark params unused in the body. *)
let wrap_type_params ~loc ?(prefix = "") params body =
  List.fold_right
    (fun param body -> [%expr fun [%p ppat_var ~loc { txt = prefix ^ param; loc }] -> [%e body]])
    params body

(* Generate schema for a single type declaration, given the recursive type group *)
let derive_single_type ~loc ~config ~recursive_types ?extra_defs_var type_decl =
  let type_name = type_decl.ptype_name.txt in
  let allow_extra_fields = Attribute.get jsonschema_td_allow_extra_fields type_decl |> Option.is_some in
  let params = List.map (fun tp -> (get_type_param_name tp).txt) type_decl.ptype_params in
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
            let obj_schema, obj_rec =
              object_ ~loc ~config ~recursive_types ?extra_defs_var label_declarations allow_extra_fields
            in
            `Tag (name, [ obj_schema ]) :: variants, is_rec || obj_rec
          | Pcstr_tuple typs ->
            let results = List.map (type_of_core ~config ~recursive_types ?extra_defs_var) typs in
            let types = List.map fst results in
            let typs_rec = List.exists snd results in
            `Tag (name, types) :: variants, is_rec || typs_rec)
        ([], false) variants
    in
    let variants = List.rev variants in
    (* todo: raise an error if encoding is as string and constructor has a payload *)
    let schema, params_prefix =
      match config.variant_as_string with
      | true -> variant_as_string ~loc variants, "_"
      | false -> variant_as_array ~loc variants, ""
    in
    type_name, schema, is_rec, params, params_prefix
  | Ptype_record label_declarations ->
    let schema, is_rec = object_ ~loc ~config ~recursive_types ?extra_defs_var label_declarations allow_extra_fields in
    type_name, schema, is_rec, params, ""
  | Ptype_abstract ->
    (match type_decl.ptype_manifest with
    | Some core_type ->
      let schema, is_rec = type_of_core ~config ~recursive_types ?extra_defs_var core_type in
      type_name, schema, is_rec, params, ""
    | None ->
      let msg = "ppx_deriving_jsonschema: abstract type without manifest" in
      type_name, [%expr [%ocaml.error [%e estring ~loc msg]]], false, params, "")
  | Ptype_open ->
    let msg = "ppx_deriving_jsonschema: open types not supported" in
    type_name, [%expr [%ocaml.error [%e estring ~loc msg]]], false, params, ""

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
    (* First pass: determine whether the type is recursive *)
    let _, raw_schema, is_rec, params, params_prefix = derive_single_type ~loc ~config ~recursive_types type_decl in
    (* Apply with_defs before wrap_type_params so params wrap the full schema.
       For recursive types, do a second pass with extra_defs_var so hoisting
       code referencing _ppx_eds is generated in the body expression. *)
    let schema =
      if is_rec then (
        let edv = evar ~loc "_ppx_eds" in
        let _, raw_schema2, _, _, _ = derive_single_type ~loc ~config ~recursive_types ~extra_defs_var:edv type_decl in
        [%expr
          let _ppx_eds = ref [] in
          [%e Schema.with_defs ~loc type_name ~extra_defs_var:edv raw_schema2]])
      else raw_schema
    in
    let schema = wrap_type_params ~loc ~prefix:params_prefix params schema in
    [ create_value ~loc type_name schema ]
  (* Multiple type declarations (mutually recursive types) *)
  | _, type_decls when List.length type_decls > 1 ->
    (* Collect all type names in the recursive group *)
    let recursive_types = List.map (fun td -> td.ptype_name.txt) type_decls in
    let primary_type = List.hd recursive_types in
    (* Collect raw schemas without wrapping type params yet — $defs must contain
       the raw bodies so that parametric types remain valid OCaml expressions. *)
    let raw_results = List.map (fun td -> derive_single_type ~loc ~config ~recursive_types td) type_decls in
    let any_recursive = List.exists (fun (_, _, is_rec, _, _) -> is_rec) raw_results in
    if any_recursive then (
      (* Second pass: regenerate with extra_defs_var so hoisting code is emitted *)
      let edv = evar ~loc "_ppx_eds" in
      let raw_results2 =
        List.map (fun td -> derive_single_type ~loc ~config ~recursive_types ~extra_defs_var:edv td) type_decls
      in
      let defs2 = List.map (fun (name, raw, _, _, _) -> name, raw) raw_results2 in
      (* Build $defs from raw schemas, then wrap the combined schema with type params.
         Each binding gets its own _ppx_eds ref so hoisted defs don't leak across. *)
      let primary_value =
        let _, _, _, params, prefix = List.find (fun (name, _, _, _, _) -> name = primary_type) raw_results2 in
        let schema_inner = Schema.with_multi_defs ~loc ~primary_type ~extra_defs_var:edv defs2 in
        let schema =
          wrap_type_params ~loc ~prefix params
            [%expr
              let _ppx_eds = ref [] in
              [%e schema_inner]]
        in
        create_value ~loc primary_type schema
      in
      let other_values =
        List.filter_map
          (fun (name, _, _, params, prefix) ->
            if name = primary_type then None
            else (
              let schema_inner = Schema.with_multi_defs ~loc ~primary_type:name ~extra_defs_var:edv defs2 in
              let schema =
                wrap_type_params ~loc ~prefix params
                  [%expr
                    let _ppx_eds = ref [] in
                    [%e schema_inner]]
              in
              Some (create_value ~loc name schema)))
          raw_results2
      in
      primary_value :: other_values)
    else
      (* No recursion detected - wrap params and generate independent schemas *)
      List.map
        (fun (name, raw, _, params, prefix) -> create_value ~loc name (wrap_type_params ~loc ~prefix params raw))
        raw_results
  | _, _ -> [%str [%ocaml.error "ppx_deriving_jsonschema: unsupported type"]]

let generator () = Deriving.Generator.V2.make ~attributes (args ()) derive_jsonschema

let jsonschema_t ~loc = ptyp_constr ~loc { txt = Ldot (Lident "Ppx_deriving_jsonschema_runtime", "t"); loc } []

let derive_jsonschema_sig ~ctxt ast _flag_variant_as_string _flag_polymorphic_variant_tuple =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  match ast with
  | _, [ td ] ->
    let typ = combinator_type_of_type_declaration td ~f:(fun ~loc _core_type -> jsonschema_t ~loc) in
    let name = { txt = td.ptype_name.txt ^ "_jsonschema"; loc } in
    [ psig_value ~loc (value_description ~loc ~name ~type_:typ ~prim:[]) ]
  | _, type_decls when List.length type_decls > 1 ->
    List.map
      (fun td ->
        let typ = combinator_type_of_type_declaration td ~f:(fun ~loc _core_type -> jsonschema_t ~loc) in
        let name = { txt = td.ptype_name.txt ^ "_jsonschema"; loc } in
        psig_value ~loc (value_description ~loc ~name ~type_:typ ~prim:[]))
      type_decls
  | _, _ ->
    let ext = Location.error_extensionf ~loc "ppx_deriving_jsonschema: unsupported type" in
    [ psig_extension ~loc ext [] ]

let sig_generator () = Deriving.Generator.V2.make (args ()) derive_jsonschema_sig

let _ : Deriving.t = Deriving.add deriver_name ~str_type_decl:(generator ()) ~sig_type_decl:(sig_generator ())
