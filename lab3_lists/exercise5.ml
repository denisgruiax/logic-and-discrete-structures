(*5. a) Implementați funcția List.filter folosind List.fold_right, urmând exemplul dat pentru List.map .
  b) Implementați funcția List.exists care determină (returnează adevărat/fals)
  dacă există un element din listă care satisface o condiție (o funcție de element cu valoare booleană, dată ca parametru).
  Implementarea poate fi asemănătoare cu cea a funcției mem de la curs. Puteți apoi exprima List.mem folosind List.exists ?*)

let filter_list2 list_of_elements condition =
  List.fold_right
    (fun element result ->
       if condition element then element :: result else result)
    list_of_elements []
;;

filter_list2 [ 1; 2; 3; 4; 5 ] (fun element -> element mod 2 = 1);;

let rec exists_list2 condition = function
    [] -> false
  |head::tail-> condition head || exists_list2 condition tail;;

exists_list2 (fun element -> element mod 2 = 0) [1;3;3;5;7];;

let mem_list2 list_of_elements element = exists_list2 (fun element' -> element = element') list_of_elements;;

mem_list2 [1;2;3] 5;;