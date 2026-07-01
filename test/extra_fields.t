Extra fields are allowed by default (matching the Melange JSON deriver), so a
plain record produces "additionalProperties": true.

  $ cat > default.ml << 'EOF'
  > type t = { x : int } [@@deriving jsonschema]
  > EOF
  $ ./pp.exe -deriving-keep-w32 both --impl default.ml -o default.actual.ml
  $ grep additionalProperties default.actual.ml
            ("additionalProperties", (`Bool true))] in

[@@jsonschema.disallow_extra_fields] opts into strict objects with
"additionalProperties": false.

  $ cat > strict.ml << 'EOF'
  > type t = { x : int } [@@deriving jsonschema] [@@jsonschema.disallow_extra_fields]
  > EOF
  $ ./pp.exe -deriving-keep-w32 both --impl strict.ml -o strict.actual.ml
  $ grep additionalProperties strict.actual.ml
            ("additionalProperties", (`Bool false))] in

Using [@jsonschema.allow_extra_fields] and [@jsonschema.disallow_extra_fields]
together is rejected.

  $ cat > conflict.ml << 'EOF'
  > type t = { x : int } [@@deriving jsonschema] [@@jsonschema.allow_extra_fields] [@@jsonschema.disallow_extra_fields]
  > EOF
  $ ./pp.exe -deriving-keep-w32 both --impl conflict.ml -o conflict.actual.ml
  File "conflict.ml", line 1, characters 0-115:
  1 | type t = { x : int } [@@deriving jsonschema] [@@jsonschema.allow_extra_fields] [@@jsonschema.disallow_extra_fields]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: ppx_deriving_jsonschema: [@jsonschema.allow_extra_fields] and [@jsonschema.disallow_extra_fields] are mutually exclusive
  [1]
