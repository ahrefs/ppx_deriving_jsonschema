let classify f x =
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
    | JSONObject dict -> `Assoc (Array.to_list (Array.map (fun (k, v) -> k, decode v) (Js.Dict.entries dict)))
  in
  decode (f x)
