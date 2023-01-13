(*10. Scrieți o funcție care elimină duplicatele consecutive: ia ca parametru o listă și construiește o
  listă în care toate secvențele de elemente consecutive egale au fost înlocuite cu un singur element.
  Indicație: puteți identifica prin tipar fragmente cu două elemente egale: e1 :: e2 :: t when e1 = e2*)

let duplicates list =
  let rec duplicates' accumulator = function
    | [] -> List.rev accumulator
    | [ item ] -> duplicates' (item :: accumulator) []
    | item :: item2 :: tail ->
      if item = item2 then duplicates' (item :: accumulator) tail
      else duplicates' (item :: accumulator) (item2 :: tail)
  in
  duplicates' [] list
  ;;

  duplicates [ 1; 1; 2; 2; 3; 4; 5; 5; 6; 7; 7; 8; 8; 9; 10; 10 ];;