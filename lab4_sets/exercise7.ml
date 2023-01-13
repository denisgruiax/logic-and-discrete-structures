(*
  7. Un număr natural descompus în factori primi poate fi reprezentat ca mulțime de perechi (factor prim, exponent), 
de exemplu {(2, 3), (5, 1)} pentru 40.
Descrieți, în notație matematică pe hârtie, cum obțineți în cazul general mulțimea tuturor divizorilor unui număr astfel dat, 
folosind operații pe mulțimi. Calculați mulțimea divizorilor pentru un număr cu cel puțin 3 factori primi, 
din care cel puțini doi cu exponenți mai mari decât 1.
Scrieți, folosind liste sau mulțimi, o funcție care implementează unul din pașii de calcul găsiți rezolvând punctul anterior.
*)

module Int2D' = struct
  type t = int * int

  let compare = compare
end

module Int2D = Set.Make (Int2D')

let prime_factor number =
  let rec prime_factor' number divisor (x, y) counter acc =
    match number with
    | 1 -> if y > 0 then Int2D.add (x, y) acc else acc
    | _ ->
      if number mod divisor = 0 then
        prime_factor' (number / divisor) divisor
          (divisor, counter + 1)
          (counter + 1) acc
      else
        prime_factor' number (divisor + 1) (divisor, counter) 0
          (if y > 0 then Int2D.add (x, y) acc else acc)
  in
  prime_factor' number 2 (0, 0) 0 Int2D.empty

let print_pair (x, y) =
  print_char '(';
  Printf.printf "%d, %d), " x y;
  Printf.printf "\b\b"
;;

Int2D.iter print_pair (prime_factor 40);;