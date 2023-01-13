(*
  9. Scrieți o funcție care ia ca parametru un dicționar reprezentând o funcție parțială f de la șiruri la șiruri 
  și calculează pentru un șir s numărul maxim n pentru care fn(s) e definit, respectiv generează o excepție 
  dacă șirul fn(s) e ciclic (definit pentru orice n).

  De exemplu, pentru f("a") = "b", f("b") = "c", f("d") = "e", f("e") = "f", f("f") = "e", 
  avem depth("x") = 0, depth("a") = 2, depth("b") = 1, iar depth("d") generează excepție (la fel pentru "e" și "f").
  Indicație: La parcurgerea pornind de la s rețineți mulțimea tuturor șirurilor 
  deja întâlnite pentru a detecta un eventual ciclu.
*)

module Set_of_Strings = Set.Make (String)
module Dictionary = Map.Make (String)

let dictionary =
  Dictionary.(
    singleton "a" "b" |> add "b" "c" |> add "d" "e" |> add "e" "f"
    |> add "f" "e")

let depth2 dictionary str =
  let rec depth2' dictionary str acc =
    match Dictionary.mem str dictionary with
    | false -> acc
    | _ -> depth2' dictionary (Dictionary.find str dictionary) (acc + 1)
  in
  depth2' dictionary str 0
;;

depth2 dictionary "a";;

let depth dictionary str = Dictionary.fold (fun key value acc ->
    if key = str 
      then  
      
      else
) dictionary 0;;