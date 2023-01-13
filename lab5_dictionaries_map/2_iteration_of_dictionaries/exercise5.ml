(*
  7. Scrieți o funcție care primește o funcție și un dicționar și returnează maximul valorilor funcției 
  pentru toate intrările dicționarului, sau generează excepția Not_found pentru un dicționar vid.
Funcția-parametru are ca argumente cheia și valoarea unei intrări, și poate returna valori arbitrare. 
Folosiți fold pentru parcurgere, și max (definită implicit pentru orice tip) pentru a compara valorile returnate de 
funcția parametru.
*)

module Dictionary = Map.Make (String)

let max_of_values max_function dictionary =
  if Dictionary.is_empty dictionary then raise Not_found
  else
    Dictionary.fold
      (fun key value res ->
         let temp_value = max_function value in Printf.printf "%d\n" temp_value;
         if temp_value > res then temp_value else res)
      dictionary min_int;;

let test_dictionary = Dictionary.(singleton "x" 1 |> add "y" 3 |> add "z" 5);;

max_of_values (fun value -> Random.int value) test_dictionary;;