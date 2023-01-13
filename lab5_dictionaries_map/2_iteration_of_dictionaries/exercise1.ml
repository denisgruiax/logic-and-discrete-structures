(*
  3. Implementați cu ajutorul lui fold din modulul Map funcția filter care creează un nou 
dicționar doar cu perechile din dicționarul dat care satisfac o funcție dată.
Documentația specifică tipurile pentru funcțiile fold și filter. 
Ele funcționează similar ca pentru mulțimi, dar funcția dată ca prim argument 
are ca parametri atât cheia cât și valoarea intrării curente din dicționar (iar pentru fold 
și acumulatorul pentru rezultat). Ordinea parametrilor e aceeași ca la Set: (1) funcția, 
(2) colecția prelucrată (dicționarul), iar pentru fold și (3) valoarea inițială.
*)

module Dictionary = Map.Make (String)

let dictionary_filter condition dictionary =
  Dictionary.fold
    (fun key value acc ->
       if condition value then Dictionary.add key value acc else acc)
    dictionary Dictionary.empty;;

let test = dictionary_filter (fun elt -> elt mod 2 = 0) (Dictionary.(singleton "x" 2 |> add "y" 1));;
Dictionary.iter (Printf.printf "%s %d\n") (test);;