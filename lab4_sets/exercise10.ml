(*
  10. Un calculator memorează numere în doi regiștri a și b și poate executa la fiecare pas una din două instrucțiuni: a += b 
care adună pe b la a (modificând pe a) și b += a (analog). Scrieți o funcție care ia ca parametri două numere naturale 
a și b și o valoare limită lim și calculează mulțimea tuturor numerelor care se pot obține cu aceste 
instrucțiuni pornind de la a și b, nedepășind lim .
De exemplu, dacă inițial (a,b)=(3,1) și lim=5, în primul pas se ajunge fie la (4,1) fie la (3,4). 
Din (3, 4) nu se poate continua (s-ar depăși limita); din (4, 1) se ajunge la (4, 5) și (5, 1). Răspunsul e {1, 3, 4, 5}.
Puteți lucra acumulând o mulțime de numere (direct rezultatul cerut) sau o mulțime de perechi 
obținute până la un moment dat, din care extrageți apoi mulțimea numerelor. Varianta a doua e mai generală, 
funcționează și când spațiul stărilor are cicluri (de exemplu, dacă am avea și 
instrucțiunile a-=b și b-=a cât timp numerele sunt nenegative). 
Cum mulțimea perechilor obținute crește la fiecare pas, dar e finită (numerele sunt limitate), 
puteți adapta funcția de punct fix de aici, folosind equal pentru a compara mulțimi.
Puteți lucra cu mulțimi de perechi de întregi definind modulul
*)

module IntSet' = struct
  type t = int
  let compare = compare
end

module IntSet= Set.Make(IntSet');;

module IntPair' = struct
  type t = int * int

  let compare = compare
end

module IntPair = Set.Make (IntPair')

let print_pair (a, b) = Printf.printf "(%d %d)\n" a b

let computer_registers (a, b) lim =
  let rec computer_registers' (a, b) lim =
    let sum = a + b in
    match sum with
    | 0 -> IntPair.singleton (a, b)
    | sum when sum > lim -> IntPair.singleton (a, b)
    | _ ->
      IntPair.add (a, sum) (IntPair.add (sum, b) (computer_registers' (sum, b) lim))
in IntPair.fold (fun (a, b) res-> IntSet.add a (IntSet.add b res)) (computer_registers' (a, b) lim) (IntSet.empty);;
;;

IntSet.iter (Printf.printf "%d ") (computer_registers (3, 1) 5);;
