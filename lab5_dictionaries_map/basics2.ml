module IntSet = Set.Make (struct
    type t = int

    let compare = compare
  end)

module Dictionary = Map.Make(String);;

let find key dictionary = try Dictionary.find key dictionary with Not_found -> IntSet.empty;;

IntSet.iter print_int (find "denis" (Dictionary.(singleton "denis" IntSet.(singleton 5 |> add 1))));;

let setmap_of_assoc lst =
  let addpair m (a, b) = Dictionary.add a (find a m |> IntSet.add b) m
  in List.fold_left addpair Dictionary.empty lst;;
(*
  first wee have the function pair, at middle where find search the "a" key in "m" dictionary, it will return the set of "a" key
and will add to it the "b" value and will create a new "m" dictionary with key a and the new set that contains "b" value

in List.fold_left
*)

let m = setmap_of_assoc [("x", 3); ("y", 6); ("z", 1); ("y", 5)];;

IntSet.iter print_int (find "y" m);;

Dictionary.add "x" 5 Dictionary.empty;;

Dictionary.bindings m;;

Dictionary.bindings m |> List.map (fun (a, b) -> (a, IntSet.elements b));; 