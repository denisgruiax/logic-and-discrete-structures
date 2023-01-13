(*6. a) Implementați folosind List.fold_left o funcție countif care ia ca parametru o funcție f cu
  valori boolene și o listă și returnează numărul de elemente pentru care funcția f e adevărată.
  b) Implementați similar o funcție sumif care calculează suma tuturor elementelor (presupuse întregi)
  pentru care funcția f e adevărată.*)

let count_if condition list_of_items =
  List.fold_left
    (fun accumulator item ->
       if condition item then accumulator + 1 else accumulator)
    0 list_of_items
;;

count_if (fun item -> item mod 2 = 0) [ 1; 2; 3; 4; 5; 6 ];;

let sum_if condition list_of_items =
  List.fold_left
    (fun accumulator item ->
       if condition item then accumulator + item else accumulator) 0
    list_of_items
;;

sum_if (fun item -> item mod 2 = 0) [1;2;3;4;5;6];;
