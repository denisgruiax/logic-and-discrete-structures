(*
  6. Scrieți o funcție care primește un dicționar de la șiruri la întregi și o listă de șiruri și 
  returnează mulțimea tuturor valorilor din dicționar care corespund șirurilor din listă.
  Parcurgeți lista cu List.fold_left. Când folosiți find, tratați excepția Not_found pentru a 
  adăuga o valoare la mulțimea-acumulator doar când cheia e găsită în dicționar.
*)

module IntSet = Set.Make (struct
    type t = int

    let compare = compare
  end)

module Dictionary = Map.Make (String)

let set_from_dictionary dictionary lst =
  List.fold_left
    (fun res elt ->
       let value = try Dictionary.find elt dictionary with Not_found -> 0 in
       if value > 0 then IntSet.add value res else res)
    IntSet.empty lst
;;

IntSet.iter(Printf.printf "%d ") (set_from_dictionary (Dictionary.(singleton "x" 1 |> add "y" 2)) ["x";"y" ;"z"]);;