(*
9. Scrieți o funcție care ia ca parametru o funcție (de doi întregi cu rezultat întreg) 
și două mulțimi de întregi A și B și returnează mulțimea valorilor f a b cu a ∈ A și b ∈ B. 
Adaptați parcurgerile făcute pentru produsul cartezian.
*)

module IntSet' = struct
  type t = int

  let compare = compare
end

module IntSet = Set.Make (IntSet')

module IntPair' = struct
  type t = int * int

  let compare = compare
end

module IntPair = Set.Make (IntPair')

let print_pair (a, b) = Printf.printf "(%d %d)\n" a b

let print_set_of_pair set_of_pair =
  Printf.printf "{";
  IntPair.iter print_pair set_of_pair;
  Printf.printf "}\n"

let a = IntSet.(singleton 1 |> add 2 |> add 3)
let b = IntSet.(singleton 4 |> add 5 |> add 6)
let c = IntPair.(singleton (1, 2) |> add (3, 4))

let cartesian_product a b =
  IntSet.fold
    (fun elt res ->
       IntSet.fold (fun elt' res' -> IntPair.add (elt, elt') res') b res)
    a IntPair.empty

let d = cartesian_product a b;;

IntPair.iter print_pair d;;

let function_on_cartesian_product f a b =
  let cs = cartesian_product a b in
  IntPair.fold (fun elt res -> IntSet.add (f elt) res) cs IntSet.empty;;

IntSet.iter (Printf.printf "%d ") (function_on_cartesian_product (fun (a, b) -> a+b) a b);;

IntSet.equal a b;;