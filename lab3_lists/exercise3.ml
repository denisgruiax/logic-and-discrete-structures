(*3. a) Implementați funcția List.nth care returnează al n-lea element dintr-o listă.
  Observați întâi în interpretor comportamentul funcției standard pentru valori ale lui n invalide
  (negative) sau prea mari (mai mari decât lungimea listei). Puteți produce excepția Invalid_argument
  mesaj apelând funcția invalid_arg mesaj, și excepția Failure mesaj apelând funcția failwith mesaj.
  b) Implementați o funcție firstn care returnează o listă cu primele n elemente dintr-o listă dată.*)

let nth_element list_of_elements index =
  if index < 0 then invalid_arg "Index is less than 0!"
  else if index > List.length list_of_elements then
    failwith "Index greater than length of list!"
  else
    let rec nth_element list_of_elements index =
      match list_of_elements with
      | [] -> failwith "Empty list!"
      | head :: tail -> if index = 0 then head else nth_element tail (index - 1)
    in
    nth_element list_of_elements index
;;

nth_element [ 1; 2; 3 ] 1;;

let first_n_elements list_of_elements size =
  if size < 0 then invalid_arg "Number less than 0!"
  else if (size+1) > List.length list_of_elements then
    failwith "Size of new list longer than size of old list!"
  else
    let rec first_n_elements' list_of_elements size accumulator =
      match list_of_elements with
      | [] -> failwith "Empty list!"
      | head :: tail ->
        if size = 0 then List.rev (head::accumulator)
        else first_n_elements' tail (size - 1) (head :: accumulator)
    in
    first_n_elements' list_of_elements size [];;

first_n_elements [1;2;3;4;5;6;7;8;9] 9;;

let x = (@);;

x [1;2] [3;4];;