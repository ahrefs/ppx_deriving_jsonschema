melange schemas
  $ node ./output/test/melange/generate_schemas.js > test_schemas.actual.json
  $ python3 - <<'EOF' > test_schemas.actual.pretty.json
  > import json
  > from pathlib import Path
  > actual = Path("test_schemas.actual.json").read_text()
  > actual = actual.replace("file://test/melange/cases.ml:", "file://test/cases.ml:")
  > print(json.dumps(json.loads(actual), indent=2))
  > EOF
  $ python3 - <<'EOF' > test_schemas.expected.pretty.json
  > import json
  > from pathlib import Path
  > print(json.dumps(json.loads(Path("../test_schemas.expected.json").read_text()), indent=2))
  > EOF
  $ diff -u test_schemas.expected.pretty.json test_schemas.actual.pretty.json
