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
  > EOF
  $ ./pp.exe -deriving-keep-w32 both --intf test_sig.mli -o test_sig_out.mli && cat test_sig_out.mli
  type event = {
    name: string ;
    count: int }[@@deriving jsonschema]
  include
    sig
      [%%ocaml.error
        "Ppxlib.Deriving: 'jsonschema' is not a supported signature type deriving generator"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type kind =
    | Success 
    | Error [@@deriving jsonschema]
  include
    sig
      [%%ocaml.error
        "Ppxlib.Deriving: 'jsonschema' is not a supported signature type deriving generator"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type alias = event[@@deriving jsonschema]
  include
    sig
      [%%ocaml.error
        "Ppxlib.Deriving: 'jsonschema' is not a supported signature type deriving generator"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type 'a wrapper = {
    value: 'a }[@@deriving jsonschema]
  include
    sig
      [%%ocaml.error
        "Ppxlib.Deriving: 'jsonschema' is not a supported signature type deriving generator"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  type ('a, 'b) pair = {
    first: 'a ;
    second: 'b }[@@deriving jsonschema]
  include
    sig
      [%%ocaml.error
        "Ppxlib.Deriving: 'jsonschema' is not a supported signature type deriving generator"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
