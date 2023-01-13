(*7. Implementați funcțiile List.split și List.combine care transformă o listă de perechi într-o pereche de liste, și invers.
  Indicații: urmăriți din modulul List tipurile pe care trebuie să le aibă cele două funcții.
  Scrieți tipare care combină liste și perechi, de exemplu (h1::t1, h2::t2) se potrivește cu o pereche de liste, ambele nevide. (a,b)::t identifică prima pereche dintr-o listă nevidă de perechi.
  Pentru funcția split, va trebui să folosiți rezultatul apelului recursiv, adică o pereche de două liste. Și în let se pot folosi tipare: let (l1, l2) = split t in ...
  Pentru funcția combine, care ia două argumente, le puteți combina pentru a folosi potrivirea de tipare pe perechi: let rec combine l1 l2 = match (l1, l2) with ...*)

let rec split2 = function
  | [] -> ([], [])
  | (item1, item2) :: list_of_pairs ->
    let result1, result2 = split2 list_of_pairs in
    (item1 :: result1, item2 :: result2)
;;

split2 [ (1, 2); (3, 4); (5, 6) ];;

let rec combine2 = function
  | [], [] -> []
  | head :: tail, head2 :: tail2 -> (head, head2) :: combine2 (tail, tail2)
  | _, _ -> invalid_arg "Invalid argument!";;

combine2 ([1;3;5],[2;4;6]);;
