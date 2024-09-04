type t_record = {
  f1: string }
type t_variant =
  | Aa 
  | Bb [@@deriving jsonschema]
include
  struct
    let t_variant_jsonschema =
      `Assoc
        [("type", (`String "string"));
        ("enum", (`List [`String "Aa"; `String "Bb"]))][@@warning "-32"]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
let print_record () = let t = { f1 = "hello world" } in print_endline t.f1
let print_variant () = match (Aa, Bb) with | _ -> print_endline "variant"
let () = print_record (); print_variant (); ()
