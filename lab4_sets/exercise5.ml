(*
5. Scrieți o funcție care ia o mulțime de mulțimi (de exemplu, de șiruri), 
și returnează reuniunea (variantă: intersectia) mulțimilor.
*)

(*{{}, {}, {}}*)
module StringSet = Set.Make (String)
module SetOfStringSet = Set.Make (StringSet)

let s1 =
  StringSet.empty |> StringSet.add "1" |> StringSet.add "2" |> StringSet.add "3"

let s2 =
  StringSet.empty |> StringSet.add "3" |> StringSet.add "4" |> StringSet.add "5"

let s3 =
  StringSet.empty |> StringSet.add "3" |> StringSet.add "7" |> StringSet.add "8"

let s =
  SetOfStringSet.empty |> SetOfStringSet.add s1 |> SetOfStringSet.add s2
  |> SetOfStringSet.add s3

let union_of_string_sets set_of_string_sets =
  SetOfStringSet.fold
    (fun elt res ->
       StringSet.fold (fun elt' res' -> StringSet.add elt' res') elt res)
    set_of_string_sets StringSet.empty
;;

StringSet.iter (Printf.printf "%s ") (union_of_string_sets s);;

let intersection_of_multiple_string_sets set_of_string_sets =
  let intersection_of_2_string_sets set set2 =
    StringSet.fold
      (fun elt res ->
         if StringSet.mem elt set2 then StringSet.add elt res else res)
      set StringSet.empty
  in
  let min_set = SetOfStringSet.min_elt set_of_string_sets in
  let subset = SetOfStringSet.remove min_set set_of_string_sets in
  SetOfStringSet.fold
    (fun elt res ->
       let first_intersection = intersection_of_2_string_sets elt min_set in
       StringSet.fold
         (fun elt' res' -> StringSet.add elt' res')
         first_intersection res)
    subset StringSet.empty;;

let intersection_of_multiple_string_sets set_of_string_sets =
  let intersection_of_2_string_sets set set2 =
    StringSet.fold
      (fun elt res ->
         if StringSet.mem elt set2 then StringSet.add elt res else res)
      set StringSet.empty
  in
  let min_string_set = SetOfStringSet.min_elt set_of_string_sets in
  let max_string_set = SetOfStringSet.max_elt set_of_string_sets in
  let first_intersection =
    intersection_of_2_string_sets min_string_set max_string_set
  in
  let sub_set_of_string_sets =
    SetOfStringSet.(
      set_of_string_sets |> remove min_string_set |> remove max_string_set)
  in
  SetOfStringSet.fold
    (fun elt res ->
       StringSet.fold
         (fun elt' res' -> StringSet.add elt' res)
         (intersection_of_2_string_sets elt first_intersection)
         res)
    sub_set_of_string_sets StringSet.empty
;;

StringSet.iter (Printf.printf "%s ") (intersection_of_multiple_string_sets s);;

(*
     How each set must be compared to each set, i will no do O(n^4), i will directly intersect each list in the "sum",
   so, the conclusion i mean to say is this, if an element will be added to the list, it should be found in each set, because the problem
   i understand it do the intersection of whole sets.

     If we have {"1", "2"} {"2", "3"} {"3", "4"} -> result {}
     If we have {"1", "2"} {"2", "3"} {"2", "4"} -> {"2"}
     If we have {}, {....}, {.....}, {....} -> {} because an intersection with the epmty set is always an empty set.
*)
