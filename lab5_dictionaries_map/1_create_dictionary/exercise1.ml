(*
  1. Scrieți o funcție care ia o listă de asociere cu perechi de tip (șir, întreg) 
și creează un dicționar în 
care fiecare șir e asociat cu suma tuturor valorilor cu care e asociat în listă.
*)

module Dictionary = Map.Make (String);;

let test = [ ("x", 2); ("y", 3); ("z", 4); ("y", 2) ]

let sum_of_values_from_list_of_pairs lst =
  let add_pair dictionary (key, value) =
    Dictionary.add key
      ((try Dictionary.find key dictionary with Not_found -> 0)
       + value)
      dictionary
  in

  List.fold_left add_pair Dictionary.empty lst;;

Dictionary.iter (Printf.printf "%s %d\n") (sum_of_values_from_list_of_pairs test);;