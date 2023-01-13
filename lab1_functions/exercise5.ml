(*Exercițiul 5: Mediana
  Scrieți o funcție care calculează mediana a trei valori (valoarea aflată între celelalte două).
  Încercați să scrieți cod cât mai simplu, și să nu-l repetați. Puteți folosi o funcție auxiliară care calculează
  mediana a trei numere, pentru care știm că primul e mai mic sau egal decât al doilea. Sau puteți încerca
  să compuneți doar funcțiile standard max/min de două elemente (expresia trebuie să fie oarecum
  simetrică). Care din variante necesită mai puține comparații? *)

let median arg arg2 arg3 =
  min (min (max arg arg2) (max arg arg3)) (max arg2 arg3)
;;

(*6 comparations*)

median 2 1 3;;
median 2 3 1;;
median 1 2 3;;
median 3 2 1;;
median 1 3 2;;
median 3 1 2;;

let min' a b c = min a (min b c)
let max' a b c = max a (max b c)

let median' a b c =
  if a > min' a b c && a < max' a b c then Printf.printf "The median is %d" a;
  if b > min' a b c && b < max' a b c then Printf.printf "The median is %d" b;
  if c > min' a b c && c < max' a b c then Printf.printf "The median is %d" c;;
  (*18 comparations*)

median' 2 1 3;;
median' 2 3 1;;
median' 1 2 3;;
median' 3 2 1;;
median' 1 3 2;;
median' 3 1 2;;