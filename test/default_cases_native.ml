let%expect_test "default_cases_shared" =
  Generate_schemas_cases.run_default_checks print_endline;
  [%expect
    {|
    ok: default_score
    ok: default_label
    ok: default_speed
    ok: default_is_active
    ok: default_pair
    ok: default_pairs
    ok: default_variant
    ok: default_record
    ok: default_int_list
    ok: default_empty_list
    ok: module_t_default
    |}]
