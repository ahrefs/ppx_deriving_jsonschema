let classify f x =
  (* [Js.Json.classify] only checks for [=== null] when distinguishing null from object, so it
     mis-classifies [undefined] as [JSONObject]. melange-json's [@option] [@drop_default] codegen
     emits [Js.Undefined.empty] for [None], so an [_to_json] result can legitimately contain
     [undefined] values that we have to handle here. We treat them as absent fields, matching
     [JSON.stringify] (which drops undefined object members and turns undefined array elements
     into null). *)
  let is_undefined json = Js.typeof json = "undefined" in
  let rec decode json =
    if is_undefined json then `Null
    else (
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
        `Assoc
          (Js.Dict.entries dict
          |> Array.to_list
          |> List.filter_map (fun (k, v) -> if is_undefined v then None else Some (k, decode v))))
  in
  decode (f x)
