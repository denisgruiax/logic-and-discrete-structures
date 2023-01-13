(*
4. Implementați funcția standard partition care ia ca parametri o funcție booleană f și 
o mulțime s și returnează o pereche de mulțimi, 
cu elementele din s care satisfac, respectiv nu satisfac funcția f.
*)

module Int = struct
  type t = int

  let compare = compare
end

module IntSet = Set.Make (Int)

let print_int_set set =
  if IntSet.cardinal set > 0 then (
    print_char '{';
    IntSet.iter (Printf.printf "%d, ") set;
    Printf.printf "\b\b}")
  else print_string "{}"

let print_pair_intset (set, set2) =
  print_string "set: ";
  print_int_set set;
  print_string "\nset2: ";
  print_int_set set2;;

let partition_set_of_int f set =
  IntSet.fold
    (fun elt (res, res2) ->
       if f elt then (IntSet.add elt res, res2) else (res, IntSet.add elt res2))
    set
    (IntSet.empty, IntSet.empty);;

let set = IntSet.(empty|>add 1 |> add 2 |> add 3 |> add 4 |> add 5 |> add 6 |> add 7 |> add 8 |> add 9 |> add 10);;

print_pair_intset (partition_set_of_int (fun elt -> elt mod 3 == 0) set);;