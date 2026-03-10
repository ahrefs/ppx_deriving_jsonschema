  $ cat > test_sig.mli << 'EOF'
  > type event = {
  >   name : string;
  >   count : int;
  > }
  > [@@deriving jsonschema]
  > 
  > type kind =
  >   | Success
  >   | Error
  > [@@deriving jsonschema]
  > 
  > type alias = event [@@deriving jsonschema]
  > 
  > type 'a wrapper = { value : 'a } [@@deriving jsonschema]
  > 
  > type ('a, 'b) pair = {
  >   first : 'a;
  >   second : 'b;
  > }
  > [@@deriving jsonschema]
  > 
  > type foo = { bar : bar option }
  > and bar = { foo : foo option }
  > [@@deriving jsonschema]
  > EOF
  $ ./pp.exe -deriving-keep-w32 both --intf test_sig.mli -o test_sig_out.mli && cat test_sig_out.mli
  type event = {
    name: string ;
    count: int }[@@deriving jsonschema]
  include sig val event_jsonschema : Melange_json.t end[@@ocaml.doc "@inline"]
  [@@merlin.hide ]
  type kind =
    | Success 
    | Error [@@deriving jsonschema]
  include sig val kind_jsonschema : Melange_json.t end[@@ocaml.doc "@inline"]
  [@@merlin.hide ]
  type alias = event[@@deriving jsonschema]
  include sig val alias_jsonschema : Melange_json.t end[@@ocaml.doc "@inline"]
  [@@merlin.hide ]
  type 'a wrapper = {
    value: 'a }[@@deriving jsonschema]
  include sig val wrapper_jsonschema : Melange_json.t -> Melange_json.t end
  [@@ocaml.doc "@inline"][@@merlin.hide ]
  type ('a, 'b) pair = {
    first: 'a ;
    second: 'b }[@@deriving jsonschema]
  include
    sig
      val pair_jsonschema : Melange_json.t -> Melange_json.t -> Melange_json.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type foo = {
    bar: bar option }
  and bar = {
    foo: foo option }[@@deriving jsonschema]
  include
    sig val foo_jsonschema : Melange_json.t val bar_jsonschema : Melange_json.t
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
