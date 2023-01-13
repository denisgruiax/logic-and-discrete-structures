(*
3. Implementați funcția standard filter care ia ca parametri o funcție booleană (condiție, predicat) f 
și o mulțime s și returnează mulțimea elementelor din s care satisfac funcția f.
*)

module Int = struct
  type t = int

  let compare = compare
end

module IntSet = Set.Make (Int)

let set_of_intlist lst =
  List.fold_left (fun res elt -> IntSet.add elt res) IntSet.empty lst

let print_int_set set =
  if IntSet.cardinal set > 0 then (
    print_char '{';
    IntSet.iter (Printf.printf "%d, ") set;
    Printf.printf "\b\b}")
  else print_string "{}"

let filter_set f s =
  IntSet.fold
    (fun elt res -> if f elt then IntSet.add elt res else res)
    s IntSet.empty

let x = filter_set (fun x -> x mod 2 = 0) (IntSet.add 2 (IntSet.singleton 1));;

print_int_set x;;
