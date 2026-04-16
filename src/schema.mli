val const : loc:Warnings.loc -> string -> Ppxlib.expression
val type_ref : loc:Warnings.loc -> string -> Ppxlib.expression
val type_def : loc:Warnings.loc -> string -> Ppxlib.expression
val null : loc:Warnings.loc -> Ppxlib.expression
val char : loc:Warnings.loc -> Ppxlib.expression
val oneOf : loc:Warnings.loc -> Ppxlib.expression list -> Ppxlib.expression
val anyOf : loc:Warnings.loc -> Ppxlib.expression list -> Ppxlib.expression
val array_ : loc:Warnings.loc -> ?min_items:int -> ?max_items:int -> Ppxlib.expression -> Ppxlib.expression
val tuple : loc:Warnings.loc -> Ppxlib.expression list -> Ppxlib.expression
val enum : loc:Warnings.loc -> string option -> Ppxlib.expression list -> Ppxlib.expression
val enum_string : loc:Warnings.loc -> string list -> Ppxlib.expression
val nullable : loc:Warnings.loc -> Ppxlib.expression -> Ppxlib.expression
val annotation : loc:Warnings.loc -> string * Ppxlib.expression -> Ppxlib.expression -> Ppxlib.expression
val format : loc:Warnings.loc -> string -> Ppxlib.expression -> Ppxlib.expression
val maximum : loc:Warnings.loc -> Ppxlib.expression -> Ppxlib.expression -> Ppxlib.expression
val minimum : loc:Warnings.loc -> Ppxlib.expression -> Ppxlib.expression -> Ppxlib.expression
val description : loc:Warnings.loc -> string -> Ppxlib.expression -> Ppxlib.expression
val variants :
  loc:Warnings.loc ->
  ?as_string:bool ->
  [< `Inherit of Ppxlib.expression | `Tag of string * Ppxlib.expression list * string option ] list ->
  Ppxlib.expression

module Annotation : sig
  val add_format :
    loc:Warnings.loc ->
    ('a, string Location.loc) Ppxlib.Attribute.t * 'a ->
    Ppxlib.core_type ->
    Ppxlib.expression ->
    Ppxlib.expression
  val add_maximum :
    loc:Warnings.loc ->
    ('a, Ppxlib.expression) Ppxlib.Attribute.t * 'a ->
    Ppxlib.core_type ->
    Ppxlib.expression ->
    Ppxlib.expression
  val add_minimum :
    loc:Warnings.loc ->
    ('a, Ppxlib.expression) Ppxlib.Attribute.t * 'a ->
    Ppxlib.core_type ->
    Ppxlib.expression ->
    Ppxlib.expression
  val add_description :
    loc:Warnings.loc -> ('a, string Location.loc) Ppxlib.Attribute.t * 'a -> Ppxlib.expression -> Ppxlib.expression
  val add_annotations :
    loc:Warnings.loc ->
    ?core_type:Ppxlib.core_type ->
    Ppxlib.expression option ->
    Ppxlib.expression ->
    Ppxlib.expression
end
