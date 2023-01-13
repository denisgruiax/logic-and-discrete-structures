(*
1. Scrieți o funcție care ia ca parametru o mulțime de șiruri de caractere și o tipărește, folosind iteratorul 
S.iter pentru a parcurge elementele. Afișați mulțimea pe o linie, între acolade { } și cu virgulă între elemente. 
Puteți folosi print_string, print_char, print_newline (vezi modulul implicit deschis Pervasives) sau Printf.printf.
*)

module StringSet = Set.Make (String);;

let set = StringSet.(empty |> add "x" |> add "y" |> add "z");;

let print_string_set set =
  print_char '{';
  StringSet.iter (Printf.printf "%s, ") set;
  Printf.printf "\b\b}"
;;

print_string_set set;;

(*----------------------------------------------------TEST 2----------------------------------------------------*)

module Int = struct
  type t = int

  let compare = compare
end;;

module IS = Set.Make (Int);;

let set_of_intlist2 lst =
  List.fold_left (fun res e -> IS.add e res) IS.empty lst;;

let print_int_set2 intSet =
  print_char '{';
  IS.iter (Printf.printf "%d, ") intSet;
  Printf.printf "\b\b}";;

let set_of_intlist2 lst =
  List.fold_left (fun res e -> IS.add e res) IS.empty lst;;

let print_int_set3 intSet = IS.iter (Printf.printf "%d, ") intSet;;

print_int_set2 (set_of_intlist2 [ 2; 1; 7; 3; 2; 6; 8; 9 ]);;