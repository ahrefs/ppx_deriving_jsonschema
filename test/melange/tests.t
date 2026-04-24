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

melange schemas
  $ node ./output/test/melange/generate_schemas.js > test_schemas.actual.json
  $ node - <<'EOF'
  > const fs = require("node:fs");
  > const actual = JSON.stringify(JSON.parse(fs.readFileSync("test_schemas.actual.json", "utf8")));
  > const expected = JSON.stringify(JSON.parse(fs.readFileSync("../test_schemas.expected.json", "utf8")));
  > if (actual !== expected) process.exit(1);
  > EOF
