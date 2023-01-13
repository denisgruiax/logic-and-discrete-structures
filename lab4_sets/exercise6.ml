(*
    6. Scrieți o funcție care returnează mulțimea cifrelor unui număr. 
  Scrieți apoi altă funcție care ia o mulțime de numere și 
  returnează reuniunea/intersecția dintre mulțimile cifrelor lor.
*)
module Int = struct
  type t = int

  let compare = compare
end

module IntSet = Set.Make (Int);;
module SetOfIntSets = Set.Make (IntSet);;

let digits_to_set number =
  let rec digits_to_set' number acc =
    match number with
    | 0 -> acc
    | _ -> digits_to_set' (number / 10) (IntSet.add (number mod 10) acc)
  in
  digits_to_set' number IntSet.empty
;;

IntSet.iter print_int (digits_to_set 123);;

let intersection_of_2_int_sets set set2 =
  IntSet.fold
    (fun elt res -> if IntSet.mem elt set2 then IntSet.add elt res else res)
    set IntSet.empty;;

let intersection_of_set_of_sets_of_digits_of_numbers set_of_numbers =
  let min_number = IntSet.min_elt set_of_numbers in
  let max_number = IntSet.max_elt set_of_numbers in
  let first_intersection =
    intersection_of_2_int_sets (digits_to_set min_number)
      (digits_to_set max_number)
  in
  let subset_of_set_of_numbers =
    IntSet.(set_of_numbers |> remove min_number |> remove max_number)
  in
  IntSet.fold
    (fun elt res -> IntSet.fold (fun elt' res' -> IntSet.add elt' res') (intersection_of_2_int_sets (digits_to_set elt) first_intersection) res)
    subset_of_set_of_numbers IntSet.empty;;

let set_of_numbers = IntSet.(empty |> add 123 |> add 345 |> add 367);;
let result = intersection_of_set_of_sets_of_digits_of_numbers set_of_numbers;;
IntSet.iter print_int result;;

(* 
  How each set must be compared to each set, i will no do O(n^4), i will directly intersect each list in the "sum",
so, the conclusion i mean to say is this, if an element will be added to the list, it should be found in each set, because the problem
i understand it do the intersection of whole sets.

  If we have {1,2} {2,3} {3,4} -> result {}
  If we have {1,2} {2,3} {2, 4} -> {2}
  If we have {}, {....}, {.....}, {....} -> {} because an intersection with the epmty set is always an empty set.  
*)