melange schemas
  $ node ./output/test/melange/generate_schemas.js > test_schemas.actual.json
  $ node - <<'EOF'
  > const fs = require("node:fs");
  > const actual = JSON.stringify(JSON.parse(fs.readFileSync("test_schemas.actual.json", "utf8")));
  > const expected = JSON.stringify(JSON.parse(fs.readFileSync("../test_schemas.expected.json", "utf8")));
  > if (actual !== expected) process.exit(1);
  > EOF
