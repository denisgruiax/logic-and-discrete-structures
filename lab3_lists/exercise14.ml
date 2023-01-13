(*14. Sortați o listă prin interclasare (mergesort), despărțind lista în două jumătăți, sortând recursiv fiecare din ele,
  și apoi interclasând cele două liste sortate. Folosiți funcțiile din cele două probleme anterioare.*)

let merge list_pair =
  let rec merge' acc = function
    | [], list -> List.rev acc @ list
    | list, [] -> List.rev acc @ list
    | head :: tail, head2 :: tail2 ->
      if head <= head2 then merge' (head :: acc) (tail, head2::tail2)
      else merge' (head2 :: acc) (head::tail, tail2)
  in
  merge' [] list_pair
;;

let rec merge2 = function
  | list, []
  | [], list -> list
  | h1::t1, h2::t2 ->
    if h1 <= h2 then
      h1 :: merge2 (t1, h2::t2)
    else
      h2 :: merge2 (h1::t1, t2)
;;

let split_list list =
  let rec split_list' (acc, acc2) count = function
    | [] -> (List.rev acc, List.rev acc2)
    | head :: tail -> (
        match count mod 2 with
        | 0 -> split_list' (head :: acc, acc2) (count + 1) tail
        | _ -> split_list' (acc, head :: acc2) (count + 1) tail)
  in
  split_list' ([], []) 0 list;;

let rec halve = function
  | []
  | [_] as t1 -> t1, []
  | h::t ->
    let t1, t2 = halve t in
    h::t2, t1
;;

let rec merge_sort2 = function
  | ([] | [ _ ]) as list -> list
  | list ->
    let l1, l2 = split_list list in
    merge (merge_sort2 l1, merge_sort2 l2);;
;;

merge_sort2 [ 14; 1; 32; 6; 5; 3; 7; 123; 12 ];;
