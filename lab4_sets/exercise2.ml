(*
2. Asemănător cu funcția set_of_intlist, 
scrieți o funcție care ia o listă de perechi (de tip precizat) 
și returnează mulțimea elementelor de pe prima poziție din fiecare pereche 
(variante: a doua poziție; ambele poziții, dacă sunt de același tip).
*)

module Int = struct
  type t = int

  let compare = compare
end

module IntSet = Set.Make (Int)

let print_int_set intSet =
  print_char '{';
  IntSet.iter (Printf.printf "%d, ") intSet;
  Printf.printf "\b\b}"

let rec set_of_list_pair = function
  | [], [] -> IntSet.empty
  | head :: tail, [] -> IntSet.add head (set_of_list_pair (tail, []))
  | [], head :: tail -> IntSet.add head (set_of_list_pair ([], tail))
  | head :: tail, head2 :: tail2 ->
    if head > head2 then IntSet.add head (set_of_list_pair (tail, tail2))
    else IntSet.add head2 (set_of_list_pair (tail, tail2))

let set_of_pairlist2 list_pair =
  let rec set_of_list_pair' list_pair acc =
    match list_pair with
    | [], [] -> acc
    | head :: tail, [] -> set_of_list_pair' (tail, []) (IntSet.add head acc)
    | [], head :: tail -> set_of_list_pair' ([], tail) (IntSet.add head acc)
    | head :: tail, head2 :: tail2 ->
      if head > head2 then set_of_list_pair' (tail, tail2) (IntSet.add head acc)
      else set_of_list_pair' (tail, tail2) (IntSet.add head2 acc)
  in
  set_of_list_pair' list_pair IntSet.empty;;

let x = set_of_list_pair ([ 1; 2; 3; 4 ], [ 0 ]);;
let y = set_of_pairlist2 ([ 1; 2; 3; 4 ], [ 0 ]);;

print_int_set x;;
print_int_set y;;