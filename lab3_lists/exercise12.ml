(*12. Scrieți o funcție care interclasează două liste, fiecare ordonată crescător,
  adică returnează lista cu elementele din ambele liste, ordonate.
  Comparați primele elemente din ambele liste pentru a decide care va fi primul în rezultat,
  și apoi continuați cu listele rămase. Puteți da funcției ca parametru o pereche de două liste (l1, l2)
  și folosi un tipar pentru ambele elemente ale perechii:
  let rec merge = function
  | (h1 :: t1, h2 :: t2) -> if h1 < h2 then ...
  | ... alte cazuri ... -> ...*)

let merge list_pair =
  let rec merge' acc = function
    | [], list -> List.rev acc @ List.rev list
    | list, [] -> List.rev acc @ List.rev list
    | head :: tail, head2 :: tail2 ->
        if head <= head2 then merge' (head2 :: head :: acc) (tail, tail2)
        else merge' (head :: head2 :: acc) (tail, tail2)
  in
  merge' [] list_pair
;;

merge ([], []);;
merge ([ 1; 2; 3 ], [ 1; 2; 3 ]);;
merge ([ 1; 4 ], [ 1; 3 ]);;
merge ([ 1; 0 ], [ 1; 3 ]);;
merge ([ 1; 0 ], []);;
merge ([], [ 1; 3 ])
