val schema_version : string

type t =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of t list
  | `Null
  | `String of string
  ]

val classify : ('a -> t) -> 'a -> t

val json_schema :
  ?id:string ->
  ?title:string ->
  ?description:string ->
  ?definitions:'a ->
  [< `Assoc of (string * ([> `Assoc of 'a | `String of string ] as 'b)) list ] ->
  [> `Assoc of (string * 'b) list ]
