(*
  8. Scrieți o funcție care ia ca parametri două dicționare de la șiruri la șiruri reprezentând 
  funcții parțiale f1 și f2 și returnează dicționarul care reprezintă f2 ⚪ f1.
*)

module Dictionary = Map.Make (String)

let dictionary1 = Dictionary.(singleton "a" "b" |> add "e" "f" |> add "h" "i")
let dictionary2 = Dictionary.(singleton "x" "c" |> add "f" "g" |> add "i" "j")

let composition dictionary1 dictionary2 =
  Dictionary.fold
    (fun key value acc ->
       Dictionary.fold
         (fun key2 value2 acc2 ->
            match value = key2 with
            | false -> acc2
            | _ -> Dictionary.(acc2 |> add key value2))
         dictionary2 acc)
    dictionary1 Dictionary.empty;;

Dictionary.bindings (composition dictionary1 dictionary2);;