(*2. Asemănător cu funcția fromto (din notițele de curs) care
  generează lista numerelor întregi dintr-un interval dat,
  scrieți o funcție care creează lista tuturor întregilor dintr-un interval dat,
  divizibili cu o valoare dată d.
  Indicație: Găsiți cel mai mare număr divizibil din interval, și continuați pas cu pas.*)

let from_interval_mod_value left right value =
  let rec from_interval_mod_value' left right value accumulator =
    match left with
    | count when count = right -> (
        match count mod value with
        | 0 -> List.rev (count :: accumulator)
        | _ -> List.rev accumulator)
    | _ ->
      from_interval_mod_value' (left + 1) right value
        (match left mod value with
         | 0 -> left :: accumulator
         | _ -> accumulator)
  in
  from_interval_mod_value' left right value [];;

from_interval_mod_value 1 10 3;;