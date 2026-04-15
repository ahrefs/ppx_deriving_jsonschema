open Ppxlib
open Ast_builder.Default

let deriver_name = "jsonschema"

let value_name_pattern ~loc type_name = ppat_var ~loc { txt = type_name ^ "_jsonschema"; loc }

let create_value ~loc name value = [%stri let[@warning "-32-39"] [%p value_name_pattern ~loc name] = [%e value]]

(* Wraps [body] in nested lambdas, one per type parameter.
   Parametric types like [('a, 'b) t] derive as [fun a b -> <schema>],
   so callers can pass schemas for each type variable.
   Use [~prefix:"_"] to mark params unused in the body. *)
let wrap_type_params ~loc ?(prefix = "") params body =
  List.fold_right
    (fun param body -> [%expr fun [%p ppat_var ~loc { txt = prefix ^ param; loc }] -> [%e body]])
    params body

let apply_attr attr node f schema =
  match Attribute.get attr node with
  | Some v -> f v schema
  | None -> schema

(* schema_of_core_type and schema_of_poly_variant are mutually recursive.
   All other functions only call downward and use plain let. *)
let rec schema_of_core_type ~(config : Attrs.config) ?(recursive_types = []) core_type =
  let loc = core_type.ptyp_loc in
  let schema, is_rec =
    match core_type with
    | [%type: int] | [%type: int32] | [%type: nativeint] -> Schema.type_def ~loc "integer", false
    | [%type: int64] ->
      ( Schema.type_def ~loc "string"
        |> Schema.annotation ~loc ("description", [%expr `String [%e estring ~loc "int64 is represented as a string"]]),
        false )
    | [%type: float] -> Schema.type_def ~loc "number", false
    | [%type: string] | [%type: bytes] -> Schema.type_def ~loc "string", false
    | [%type: bool] -> Schema.type_def ~loc "boolean", false
    | [%type: char] -> Schema.char ~loc, false
    | [%type: unit] -> Schema.null ~loc, false
    | [%type: [%t? t] option] ->
      let s, is_rec = schema_of_core_type ~config ~recursive_types t in
      Schema.nullable ~loc s, is_rec
    | [%type: [%t? t] ref] -> schema_of_core_type ~config ~recursive_types t
    | [%type: [%t? t] list] | [%type: [%t? t] array] ->
      let t, is_rec = schema_of_core_type ~config ~recursive_types t in
      Schema.array_ ~loc t, is_rec
    | _ ->
    match core_type.ptyp_desc with
    | Ptyp_var name -> evar ~loc name, false
    | Ptyp_constr (id, args) ->
      (match id.txt with
      | Lident name when List.mem name recursive_types ->
        (* Recursive reference: emit $ref regardless of type arguments. *)
        Schema.type_ref ~loc name, true
      | _ ->
        let results = List.map (schema_of_core_type ~config ~recursive_types) args in
        let args = List.map fst results in
        let is_rec = List.exists snd results in
        let schema = type_constr_conv ~loc id ~f:(fun s -> s ^ "_jsonschema") args in
        let edv = evar ~loc "ppx_eds" in
        (match is_rec with
        | true ->
          (* Hoist $defs from the sub-schema into the parent accumulator and strip
             resource boundary markers ($defs), leaving just the $ref. *)
          ( [%expr
              match [%e schema] with
              | `Assoc ppx_pairs ->
                (match List.assoc_opt "$defs" ppx_pairs with
                | Some (`Assoc ppx_defs) ->
                  [%e edv] := ![%e edv] @ List.filter (fun (n, _) -> not (List.mem_assoc n ![%e edv])) ppx_defs
                | _ -> ());
                `Assoc (List.filter (fun (k, _) -> k <> "$defs") ppx_pairs)
              | ppx_other -> ppx_other],
            is_rec )
        | false ->
          (* Non-recursive arg (or no accumulator): add a location-based $id to mark
             the resource boundary so that any internal $defs refs resolve correctly. *)
          let unique_id = estring ~loc (Printf.sprintf "file://%s:%d" loc.loc_start.pos_fname loc.loc_start.pos_lnum) in
          ( [%expr
              match [%e schema] with
              | `Assoc pairs when List.mem_assoc "$defs" pairs ->
                `Assoc (("$id", `String [%e unique_id]) :: List.filter (fun (k, _) -> k <> "$id") pairs)
              | other -> other],
            is_rec )))
    | Ptyp_tuple types ->
      let results = List.map (schema_of_core_type ~config ~recursive_types) types in
      let ts = List.map fst results in
      let is_rec = List.exists snd results in
      Schema.tuple ~loc ts, is_rec
    | Ptyp_variant (row_fields, _, _) -> schema_of_poly_variant ~loc ~config ~recursive_types row_fields
    | _ ->
      let msg = Format.asprintf "ppx_deriving_jsonschema: unsupported type %a" Astlib.Pprintast.core_type core_type in
      [%expr [%ocaml.error [%e estring ~loc msg]]], false
  in
  let schema =
    schema
    |> apply_attr Attrs.jsonschema_ct_format core_type (fun fmt -> Schema.format ~loc fmt.txt)
    |> apply_attr Attrs.jsonschema_ct_description core_type (fun desc -> Schema.description ~loc desc.txt)
  in
  schema, is_rec

and schema_of_poly_variant ~loc ~(config : Attrs.config) ?(recursive_types = []) row_fields =
  let constrs, is_rec =
    List.fold_left
      (fun (constrs, is_rec) row_field ->
        match row_field.prf_desc with
        | Rtag (name, true, []) ->
          let name =
            match Attribute.get Attrs.jsonschema_polymorphic_variant_name row_field with
            | Some name -> name.txt
            | None -> name.txt
          in
          `Tag (name, [], None) :: constrs, is_rec
        | Rtag (name, false, [ typ ]) ->
          let name =
            match Attribute.get Attrs.jsonschema_polymorphic_variant_name row_field with
            | Some name -> name.txt
            | None -> name.txt
          in
          let raw_typs =
            match config.Attrs.polymorphic_variant_tuple with
            | true -> [ typ ]
            | false ->
            match typ.ptyp_desc with
            | Ptyp_tuple tps -> tps
            | _ -> [ typ ]
          in
          let results = List.map (schema_of_core_type ~config ~recursive_types) raw_typs in
          let typs = List.map fst results in
          let typs_rec = List.exists snd results in
          `Tag (name, typs, None) :: constrs, is_rec || typs_rec
        | Rtag (_, true, [ _ ]) | Rtag (_, _, _ :: _ :: _) ->
          Location.raise_errorf ~loc "ppx_deriving_jsonschema: polymorphic_variant/Rtag/&"
        | Rinherit core_type ->
          let typ, typ_rec = schema_of_core_type ~config ~recursive_types core_type in
          `Inherit typ :: constrs, is_rec || typ_rec
        | Rtag (_, false, []) -> assert false)
      ([], false) row_fields
  in
  let constrs = List.rev constrs in
  let v = Schema.variant ~loc ~as_string:config.Attrs.variant_as_string constrs in
  v, is_rec

(* Returns (schema_expression, is_recursive) *)
let schema_of_record ~loc ~(config : Attrs.config) ?(recursive_types = []) fields allow_extra_fields =
  let fields, required, is_rec =
    List.fold_left
      (fun (fields, required, is_rec) ({ pld_name; pld_type; pld_loc = _loc; _ } as field) ->
        let name =
          match Attribute.get Attrs.jsonschema_key field with
          | Some name -> name.txt
          | None -> pld_name.txt
        in
        let drop_required = Attribute.has_flag Attrs.jsonschema_option field in
        let type_def, field_rec =
          match Attribute.get Attrs.jsonschema_ref field with
          | Some def -> Schema.type_ref ~loc def.txt, false
          | None ->
          match pld_type with
          | [%type: [%t? inner] option] ->
            let s, r = schema_of_core_type ~config ~recursive_types inner in
            Schema.nullable ~loc s, r
          | _ -> schema_of_core_type ~config ~recursive_types pld_type
        in
        let type_def =
          type_def
          |> apply_attr Attrs.jsonschema_ld_description field (fun desc -> Schema.description ~loc desc.txt)
          |> apply_attr Attrs.jsonschema_ld_format field (fun fmt -> Schema.format ~loc fmt.txt)
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

(* Returns (schema_expression, is_recursive, params_prefix) *)
let schema_of_variants ~loc ~(config : Attrs.config) ?(recursive_types = []) variants =
  let variants, is_rec =
    List.fold_left
      (fun (variants, is_rec) ({ pcd_args; pcd_name = { txt = name; _ }; _ } as var) ->
        let name =
          match Attribute.get Attrs.jsonschema_variant_name var with
          | Some name -> name.txt
          | None -> name
        in
        let description_opt =
          match Attribute.get Attrs.jsonschema_cd_description var with
          | Some d -> Some d.txt
          | None -> None
        in
        match pcd_args with
        | Pcstr_record label_declarations ->
          let allow_extra_fields = Attribute.get Attrs.jsonschema_cd_allow_extra_fields var |> Option.is_some in
          let obj_schema, obj_rec =
            schema_of_record ~loc ~config ~recursive_types label_declarations allow_extra_fields
          in
          `Tag (name, [ obj_schema ], description_opt) :: variants, is_rec || obj_rec
        | Pcstr_tuple typs ->
          let results = List.map (schema_of_core_type ~config ~recursive_types) typs in
          let types = List.map fst results in
          let typs_rec = List.exists snd results in
          `Tag (name, types, description_opt) :: variants, is_rec || typs_rec)
      ([], false) variants
  in
  let variants = List.rev variants in
  let schema, params_prefix =
    match config.Attrs.variant_as_string with
    | true -> Schema.variant ~loc ~as_string:true variants, "_"
    | false -> Schema.variant ~loc variants, ""
  in
  schema, is_rec, params_prefix

(* Returns (type_name, schema_expression, is_recursive, params, params_prefix) *)
let schema_of_type_decl ~loc ~(config : Attrs.config) ~recursive_types type_decl =
  let type_name = type_decl.ptype_name.txt in
  let allow_extra_fields = Attribute.get Attrs.jsonschema_td_allow_extra_fields type_decl |> Option.is_some in
  let params = List.map (fun tp -> (get_type_param_name tp).txt) type_decl.ptype_params in
  match type_decl.ptype_kind with
  | Ptype_variant variants ->
    let schema, is_rec, params_prefix = schema_of_variants ~loc ~config ~recursive_types variants in
    type_name, schema, is_rec, params, params_prefix
  | Ptype_record label_declarations ->
    let schema, is_rec = schema_of_record ~loc ~config ~recursive_types label_declarations allow_extra_fields in
    type_name, schema, is_rec, params, ""
  | Ptype_abstract ->
    (match type_decl.ptype_manifest with
    | Some core_type ->
      let schema, is_rec = schema_of_core_type ~config ~recursive_types core_type in
      type_name, schema, is_rec, params, ""
    | None ->
      let msg = "ppx_deriving_jsonschema: abstract type without manifest" in
      type_name, [%expr [%ocaml.error [%e estring ~loc msg]]], false, params, "")
  | Ptype_open ->
    let msg = "ppx_deriving_jsonschema: open types not supported" in
    type_name, [%expr [%ocaml.error [%e estring ~loc msg]]], false, params, ""

let apply_defs ~loc = function
  | `Rec (primary, defs) ->
    let edv = evar ~loc "ppx_eds" in
    let vname name = "ppx_body_" ^ name in
    let pairs_expr =
      elist ~loc (List.map (fun (name, _) -> [%expr [%e estring ~loc name], [%e evar ~loc (vname name)]]) defs)
    in
    let base_expr =
      [%expr
        `Assoc
          [ "$defs", `Assoc ([%e pairs_expr] @ ![%e edv]); "$ref", `String [%e estring ~loc ("#/$defs/" ^ primary)] ]]
    in
    List.fold_right
      (fun (name, s) acc ->
        [%expr
          let [%p ppat_var ~loc { txt = vname name; loc }] = [%e s] in
          [%e acc]])
      defs base_expr
  | `NonRec schema ->
    let edv = evar ~loc "ppx_eds" in
    [%expr
      let ppx_result = [%e schema] in
      match ![%e edv] with
      | [] -> ppx_result
      | ppx_defs ->
      match ppx_result with
      | `Assoc ppx_pairs -> `Assoc (("$defs", `Assoc ppx_defs) :: List.filter (fun (k, _) -> k <> "$defs") ppx_pairs)
      | other -> other]

let str_type_decl ~ctxt ast flag_variant_as_string flag_polymorphic_variant_tuple =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let config : Attrs.config =
    {
      Attrs.variant_as_string = flag_variant_as_string;
      Attrs.polymorphic_variant_tuple = flag_polymorphic_variant_tuple;
    }
  in
  match ast with
  (* Single type declaration *)
  | _, [ type_decl ] ->
    let type_name = type_decl.ptype_name.txt in
    let _, raw_schema, is_rec, params, params_prefix =
      schema_of_type_decl ~loc ~config ~recursive_types:[ type_name ] type_decl
    in
    let raw_schema =
      raw_schema
      |> apply_attr Attrs.jsonschema_td_description type_decl (fun desc -> Schema.description ~loc desc.txt)
      |> apply_attr Attrs.jsonschema_td_format type_decl (fun fmt -> Schema.format ~loc fmt.txt)
    in
    let schema =
      if is_rec then
        [%expr
          let ppx_eds = ref [] in
          [%e apply_defs ~loc (`Rec (type_name, [ type_name, raw_schema ]))]]
      else
        [%expr
          let ppx_eds = ref [] in
          [%e apply_defs ~loc (`NonRec raw_schema)]]
    in
    let schema = wrap_type_params ~loc ~prefix:params_prefix params schema in
    let schema =
      match Attribute.get Attrs.jsonschema_td_description type_decl with
      | Some desc -> Schema.description ~loc desc.txt schema
      | None -> schema
    in
    [ create_value ~loc type_name schema ]
  (* Multiple type declarations (mutually recursive types) *)
  | _, type_decls when List.length type_decls > 1 ->
    let recursive_types = List.map (fun td -> td.ptype_name.txt) type_decls in
    let raw_results = List.map (schema_of_type_decl ~loc ~config ~recursive_types) type_decls in
    let any_recursive = List.exists (fun (_, _, is_rec, _, _) -> is_rec) raw_results in
    if any_recursive then
      List.map
        (fun (name, raw, _, params, prefix) ->
          let td = List.find (fun td -> td.ptype_name.txt = name) type_decls in
          let raw =
            raw
            |> apply_attr Attrs.jsonschema_td_description td (fun desc -> Schema.description ~loc desc.txt)
            |> apply_attr Attrs.jsonschema_td_format td (fun fmt -> Schema.format ~loc fmt.txt)
          in
          let defs = List.map (fun (n, r, _, _, _) -> n, if n = name then raw else r) raw_results in
          let schema =
            wrap_type_params ~loc ~prefix params
              [%expr
                let ppx_eds = ref [] in
                [%e apply_defs ~loc (`Rec (name, defs))]]
          in
          create_value ~loc name schema)
        raw_results
    else
      List.map
        (fun (name, raw, _, params, prefix) ->
          let td = List.find (fun td -> td.ptype_name.txt = name) type_decls in
          let schema =
            wrap_type_params ~loc ~prefix params
              [%expr
                let ppx_eds = ref [] in
                [%e apply_defs ~loc (`NonRec raw)]]
          in
          let schema =
            schema |> apply_attr Attrs.jsonschema_td_description td (fun desc -> Schema.description ~loc desc.txt)
          in
          create_value ~loc name schema)
        raw_results
  | _, _ -> [%str [%ocaml.error "ppx_deriving_jsonschema: unsupported type"]]

let sig_type_decl ~ctxt ast _flag_variant_as_string _flag_polymorphic_variant_tuple =
  let jsonschema_t ~loc = ptyp_constr ~loc { txt = Ldot (Lident "Ppx_deriving_jsonschema_runtime", "t"); loc } [] in
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

let _ : Deriving.t =
  Deriving.add deriver_name
    ~str_type_decl:(Deriving.Generator.V2.make ~attributes:Attrs.attributes (Attrs.args ()) str_type_decl)
    ~sig_type_decl:(Deriving.Generator.V2.make ~attributes:Attrs.attributes (Attrs.args ()) sig_type_decl)
