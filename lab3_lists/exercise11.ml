(*11. Scrieți o funcție care compară două liste după următoarea relație de ordine: o listă mai scurtă
  e "mai mică" decât una mai lungă; dacă lungimile sunt egale, ordonarea e determinată de prima pereche de elemente diferite.
  Evitați parcurgerea inutilă sau repetată a listelor. Funcția va returna un întreg negativ,
  0 sau pozitiv în funcție de ordonarea celor două liste argument.*)

let compare_2list list1 list2 =
  let length = compare (List.length list1) (List.length list2) in
  match length with
  | -1 -> -1
  | 1 -> 1
  | _ ->
    let rec compare_2list' list1 list2 =
      match (list1, list2) with
      | [], [] -> 0
      | [], _ :: _ -> -1
      | _ :: _, [] -> 1
      | head :: tail, head2 :: tail2 ->
        let result = compare head head2 in
        if result = 0 then compare_2list' tail tail2 else result
    in
    compare_2list' list1 list2;;

compare_2list ["a";"b";"c"] ["a";"b";"c"];;
