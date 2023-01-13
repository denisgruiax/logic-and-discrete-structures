(*8. Implementați funcția List.partition care ia ca parametru o funcție cu valori boolene și o listă și returnează o pereche de liste, cu elementele care satisfac, respectiv nu satisfac funcția f.
  # List.partition (fun x -> x >= 5) [4;6;7;5;4;8;9] ;;
  - : int list * int list = ([6; 7; 5; 8; 9], [4; 4])
  Puteți să o scrieți cu una din prelucrările standard? Va fi final recursivă sau nu ?
  Indicație: la fiecare pas, elementul curent se adaugă la una din listele din perechea-rezultat.*)

let partition2 condition list_of_items =
  let (result1, result2) = List.fold_left
      (fun (accumulator, accumulator2) item ->
         match condition item with
         | false -> (accumulator, item :: accumulator2)
         | true -> (item :: accumulator, accumulator2))
      ([], []) list_of_items in (List.rev result1, List.rev result2);;
;;

partition2 (fun item -> item mod 2 = 0) [1;2;3;4;5;6];;
