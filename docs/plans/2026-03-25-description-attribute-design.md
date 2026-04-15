# Design: `[@jsonschema.description]` attribute

## Goal

Add a `description` attribute to `ppx_deriving_jsonschema` that injects a `"description"` field into generated JSON Schema objects. Primary use case: LLM tool-calling, where field-level descriptions tell the model what each parameter means.

## Scope

Two contexts:

1. **Record fields** (`label_declaration`): `name: string [@jsonschema.description "The user's name"]`
2. **Top-level types** (`type_declaration`): `type t = { ... } [@@jsonschema.description "A user"]`

## Output examples

### Record field

```ocaml
type tool_params = {
  query: string [@jsonschema.description "The search query to execute"];
  max_results: int option [@jsonschema.description "Maximum results to return"];
} [@@deriving jsonschema]
```

```json
{
  "type": "object",
  "properties": {
    "query": { "type": "string", "description": "The search query to execute" },
    "max_results": { "type": "integer", "description": "Maximum results to return" }
  },
  "required": ["query"],
  "additionalProperties": false
}
```

### Top-level type

```ocaml
type tool_params = {
  query: string;
} [@@deriving jsonschema] [@@jsonschema.description "Parameters for the search tool"]
```

```json
{
  "type": "object",
  "properties": { "query": { "type": "string" } },
  "required": ["query"],
  "additionalProperties": false,
  "description": "Parameters for the search tool"
}
```

## Implementation

### 1. Declare attributes

Two new `Attribute.declare` calls following existing patterns (`jsonschema_key`, `jsonschema_ref`):

- `jsonschema_ld_description` in `label_declaration` context
- `jsonschema_td_description` in `type_declaration` context

Both accept a string payload via `Ast_pattern.(single_expr_payload (estring __'))`.

### 2. Add `Schema.with_description` helper

```ocaml
let with_description ~loc desc schema =
  [%expr match [%e schema] with
    | `Assoc fields -> `Assoc (("description", `String [%e estring ~loc desc]) :: fields)
    | s -> s]
```

This takes a schema expression and prepends a `"description"` entry. Uses runtime match since `type_of_core` returns opaque expressions.

### 3. Modify `object_` function

In the field fold, after resolving `type_def` for a field, check for the description attribute and wrap with `with_description` if present.

### 4. Modify `derive_single_type` / `derive_jsonschema`

After generating the schema for a type, check for the type-level description attribute and wrap with `with_description` if present.

### 5. Register attributes

Add both new attributes to the `attributes` list so ppxlib doesn't warn about unused attributes.

### 6. Tests

Add expect tests for:
- Record field with description
- Top-level type with description
- Both combined
- Field with description + other attributes (`@key`, `@ref`)
- Optional field with description
