# Test layout

This directory has three roles:

- snapshot the PPX expansion itself
- snapshot the generated JSON Schemas on native
- verify that Melange produces the same schema bundle

## Files in `test/`

- `shared/cases.ml`
  - source of truth for the test case type definitions and helper values
  - compiled natively and also copied into the Melange test build so both targets
    exercise the same cases
- `pp.ml`
  - standalone native PPX driver used to snapshot expansion output
- `pp_melange.ml`
  - standalone Melange-flavoured PPX driver used to snapshot expansion output
- `test.expected.ml`
  - expected native PPX expansion for `shared/cases.ml`
- `test.melange.expected.ml`
  - expected Melange-flavoured PPX expansion for `shared/cases.ml`
- `generate_schemas.ml`
  - native entrypoint that prints the full schema snapshot
- `test_schemas.expected.json`
  - expected schema snapshot shared by native and Melange
- `test_sig.t`
  - cram test for signature rewriting

## `test/shared/`

Cross-target support code.

- `cases.ml`
  - source of truth for shared case definitions
- `generate_schemas_cases.ml`
  - lists which schemas from `cases.ml` are included in the snapshot bundle
  - this is intentionally separate from `cases.ml`: `cases.ml` owns the type
    definitions, while this file owns the snapshot selection/order
- `schema_snapshot.ml`
  - tiny JSON serializer for `Ppx_deriving_jsonschema_runtime.t`
  - needed because Melange does not have `Yojson.Basic`

## `test/melange/`

Melange-only test entrypoints.

- `generate_schemas.ml`
  - emits JS that prints the shared schema snapshot
- `tests.t`
  - cram test that runs the emitted JS with Node and diffs it against
    `../test_schemas.expected.json`

## Native vs Melange

Native uses the root `test/` stanzas in `test/dune`.
Melange uses `test/melange/dune` and reuses the same cases from `shared/cases.ml`.

The important distinction is:

- native pretty-prints through `Yojson.Basic`
- Melange prints through `test/shared/schema_snapshot.ml`

The schema content should otherwise match.

## Updating tests

When adding a new case:

1. add the type/helper to `shared/cases.ml`
2. add the corresponding schema expression to `shared/generate_schemas_cases.ml`
3. refresh expectations:
   - `opam exec -- dune runtest --auto-promote`
   - if needed, regenerate `test_schemas.expected.json` from `generate_schemas.exe`
