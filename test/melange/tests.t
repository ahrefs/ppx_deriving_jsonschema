melange defaults
  $ node ./output/test/melange/melange_test.js
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

melange default schemas
  $ node ./output/test/melange/generate_default_schemas.js > default_schemas.actual.json
  $ diff -u ../default_schemas.expected.json default_schemas.actual.json
