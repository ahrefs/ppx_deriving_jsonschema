let () =
  print_endline
    (Yojson.Basic.pretty_to_string
       (Generate_schemas_cases.snapshot : Ppx_deriving_jsonschema_runtime.t :> Yojson.Basic.t))
