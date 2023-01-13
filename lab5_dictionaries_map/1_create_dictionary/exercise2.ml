(*
  2. Scrieți o funcție care ia o listă de șiruri de caractere și creează un dicționar 
  în care fiecare șir e asociat cu numărul aparițiilor din listă
*)

module Dictionary = Map.Make (String)

let test = [ "x"; "y"; "y"; "x"; "y"; "y"; "x"; "y"; "y"; "z" ];;

let count_strings_into_dictionary lst = let count_string dictionary key = Dictionary.add key ((try Dictionary.find key dictionary with Not_found -> 0) + 1) dictionary
  in List.fold_left count_string Dictionary.empty lst;;

Dictionary.iter (Printf.printf "%s %d\n") (count_strings_into_dictionary test);;