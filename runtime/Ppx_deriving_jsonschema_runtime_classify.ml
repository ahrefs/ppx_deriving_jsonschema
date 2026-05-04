type t =
  [ `Null
  | `String of string
  | `Float of float
  | `Int of int
  | `Bool of bool
  | `List of t list
  | `Assoc of (string * t) list
  ]

let classify f x =
  match%platform () with
  | Server -> f x
  | Client ->
    (* [Js.Json.classify] only checks for [=== null] when distinguishing null from object, so it
     mis-classifies [undefined] as [JSONObject]. melange-json's [@option] [@drop_default] codegen
     emits [Js.Undefined.empty] for [None], so an [_to_json] result can legitimately contain
     [undefined] values that we have to handle here. We treat them as absent fields, matching
     [JSON.stringify] (which drops undefined object members and turns undefined array elements
     into null). *)
    let rec decode json =
      match Js.Json.classify json with
      | JSONNull -> `Null
      | JSONString s -> `String s
      | JSONNumber n ->
        let i = int_of_float n in
        if float_of_int i = n then `Int i else `Float n
      | JSONFalse -> `Bool false
      | JSONTrue -> `Bool true
      | JSONArray arr -> `List (Array.to_list (Array.map decode arr))
      | JSONObject dict ->
        let is_undefined json = Js.typeof json = "undefined" in
        `Assoc
          (Js.Dict.entries dict
          |> Array.fold_left (fun acc (k, v) -> if is_undefined v then acc else (k, decode v) :: acc) [])
    in
    decode (f x)
