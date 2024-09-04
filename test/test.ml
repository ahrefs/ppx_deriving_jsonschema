type t_record = { f1 : string }

type t_variant =
  | Aa
  | Bb
[@@deriving jsonschema]

let print_record () =
  let t = { f1 = "hello world" } in
  print_endline t.f1

let print_variant () =
  match Aa, Bb with
  | _ -> print_endline "variant"

let () =
  print_record ();
  print_variant ();
  ()
