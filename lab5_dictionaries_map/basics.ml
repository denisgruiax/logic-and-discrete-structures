module Dictionary = Map.Make (String)

let dictionary =
  Dictionary.(singleton "denis" 1 |> add "alex" 2 |> add "denis" 2)
;;

Dictionary.iter (Printf.printf "%s %d\n") dictionary;;
try Dictionary.find "ale" dictionary with Not_found -> 0

(*
  uderstanding the try with and Exit exception
  for example, if we find the element in a list, we can stop the search by throwing a Exit exception 
with raise keyword
*)

let mem elt lst =
  try
    List.iter (fun elt' -> if elt' = elt then raise Exit) lst;
    false
  with Exit -> true
;;

mem 2 [ 1; 5; 3; 4 ]

exception ExcIntreg of int

let pospart lst =
  try
    List.fold_left
      (fun r e -> if e > 0 then r + e else raise (ExcIntreg r))
      0 lst
  with ExcIntreg r -> r

exception ExitLessThanZero of int

let pospart lst =
  try
    List.fold_left
      (fun res elt -> if elt > 0 then res + elt else raise (ExcIntreg res))
      0 lst
  with ExcIntreg res -> res
;;

pospart [ 1; 2; 3; -1; 5 ];;
Dictionary.add "denis" 5 dictionary

let dictionary_from_pairs lst =
  List.fold_left (fun res (a, b) -> Dictionary.add a b res) Dictionary.empty lst
;;

Dictionary.iter (Printf.printf "%s %d\n")
  (dictionary_from_pairs [ ("denis", 1); ("alex", 2); ("ionut", 3) ])
;;

Dictionary.bindings
  (dictionary_from_pairs [ ("denis", 1); ("alex", 2); ("ionut", 3) ])

(*--------------------------------------------------------------------*)


module IntSet = Map.Make (struct
    type t = int

    let compare = compare
  end);;

module Dictionary2 = Map.Make (String);;

let x = Dictionary2.singleton "denis" IntSet.(singleton 5);;

let find key dictionary = try Dictionary2.find key dictionary with Not_found -> IntSet.empty
;;