(*
  8. Scrieți o funcție care ia ca parametru o mulțime s și un număr k și returnează 
lista (sau mulțimea) tuturor submulțimilor lui s cu k elemente.
Indicație: țineți minte mulțimea aleasă deja, și numărul elementelor n din mulțimea rămasă. 
Dacă k = 0, nu mai trebuie ales nimic. Dacă k = n, trebuie luate toate elementele rămase. 
Altfel, pentru un element dat din mulțime, putem să îl alegem, sau nu.
*)

module Int = struct
  type t = int

  let compare = compare
end;;

module IntSet = Set.Make (Int);;
module Set_of_IntSet = Set.Make (IntSet);;

let print_set_of_int_sets set_of_int_sets =
  Set_of_IntSet.fold
    (fun elt res ->
       Printf.printf "{";
       IntSet.iter (Printf.printf "%d ") elt;
       Printf.printf "}\n")
    set_of_int_sets ();;

let rec powerset = function
  | [] -> [ [] ]
  | elt :: subset ->
    let powerset' = powerset subset in
    powerset' @ List.map (fun subset' -> elt :: subset') powerset';;

let powerset set =
  IntSet.fold
    (fun elt powerset' ->
       Set_of_IntSet.fold
         (fun subset -> Set_of_IntSet.add (IntSet.add elt subset))
         powerset' powerset')
    set
    (Set_of_IntSet.singleton IntSet.empty);;

let powerset2 set cardinal = let powerset2' = 
  IntSet.fold
    (fun elt powerset' ->
       Set_of_IntSet.fold
         (fun subset -> Set_of_IntSet.add (IntSet.add elt subset))
         powerset' powerset')
    set
    (Set_of_IntSet.singleton IntSet.empty)
    in Set_of_IntSet.filter (fun elt -> IntSet.cardinal elt == cardinal) powerset2'
    ;;

let int_set = IntSet.(singleton 1 |> add 2 |> add 3);;
let int_set2 = IntSet.(singleton 23 |> add 1325 |> add 126);;
let int_set3 = IntSet.(singleton 17 |> add 18 |> add 19);;

let set_of_int_sets =
  Set_of_IntSet.(singleton int_set |> add int_set2 |> add int_set3)
;;

print_set_of_int_sets (powerset int_set);;

print_set_of_int_sets
  (Set_of_IntSet.fold
     (fun sub_set -> Set_of_IntSet.add (IntSet.add 444 sub_set))
     set_of_int_sets set_of_int_sets);;

print_set_of_int_sets (powerset2 int_set 2);;