(*
  5. Implementați cu ajutorul lui fold funcția standard map care construiește un dicționar în care
  toate valorile au fost transformate folosind o funcție dată ca parametru.
*)

module Dictionary = Map.Make (String)

let dictionary_map tranform dictionary =
  Dictionary.fold
    (fun key value res -> Dictionary.add key (tranform value) res)
    dictionary Dictionary.empty

let dictionary_test = Dictionary.(singleton "x" 1 |> add "y" 2 |> add "z" 3);;

Dictionary.iter (Printf.printf "%s %d\n")
  (dictionary_map (fun elt -> elt * 2) dictionary_test);;
