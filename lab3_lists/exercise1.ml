(*1. Asemănător cu funcțiile din notițele de curs care construiesc lista cifrelor unui număr natural, scrieți:
  a) funcții care construiesc lista cifrelor unui număr care satisfac o condiție anume
  (cifre impare, pare, mai mici decât 7, etc. la alegere), în ordine normală și inversă
  b) o funcție care construiește lista cifrelor unui număr care satisfac
  o condiție dată ca parametru sub forma unei funcții cu tipul int -> bool.
  c) Invers, dată fiind o listă de cifre, construiți numărul format
  doar din cifrele care respectă o condiție (dată ca parametru funcție cu tipul int -> bool).
  Rezolvați problema direct, recursiv, și apoi prin
  compunerea lui List.filter (selectarea cifrelor) cu List.fold_left (pentru construirea numărului).*)

(*a*)
let rec reverse_digits_list = function
  | number when number < 10 -> [ number ]
  | number -> (number mod 10) :: reverse_digits_list (number / 10)
;;

reverse_digits_list 123

let digits_list number =
  let rec digits_list' number accumulator =
    match number with
    | number when number < 10 -> number :: accumulator
    | _ -> digits_list' (number / 10) ((number mod 10) :: accumulator)
  in
  digits_list' number []
;;

digits_list 123456789;;

let even_digits_list number =
  let rec even_digits_list' number accumulator =
    match number with
    | number when number < 10 -> (
        match number mod 2 with 0 -> number :: accumulator | _ -> accumulator)
    | _ ->
      even_digits_list' (number / 10)
        (match number mod 2 with
         | 0 -> (number mod 10) :: accumulator
         | _ -> accumulator)
  in
  even_digits_list' number []
;;

even_digits_list 1234567890;;

let even_digits_list number =
  let rec even_digits_list' number accumulator =
    match number with
    | number when number < 10 -> (
        match number mod 2 with 1 -> number :: accumulator | _ -> accumulator)
    | _ ->
      even_digits_list' (number / 10)
        (match number mod 2 with
         | 1 -> (number mod 10) :: accumulator
         | _ -> accumulator)
  in
  even_digits_list' number []
;;

even_digits_list 1234567890;;

let digits_less_7_list number =
  let rec digits_less_7_list' number accumulator =
    match number with
    | number when number < 10 -> (
        let digit = number mod 10 in
        match digit with
        | digit when digit < 7 -> digit :: accumulator
        | _ -> accumulator)
    | _ ->
      digits_less_7_list' (number / 10)
        (let digit = number mod 10 in
         match digit with
         | digit when digit < 7 -> digit :: accumulator
         | _ -> accumulator)
  in
  digits_less_7_list' number []
;;

digits_less_7_list 1234567890;;

let digits_list_by_condition number condition =
  let rec digits_list_by_condition' number condition accumulator =
    match number with
    | number when number < 10 -> (
        match condition number with
        | false -> accumulator
        | true -> number :: accumulator)
    | _ ->
      digits_list_by_condition' (number / 10) condition
        (let digit = number mod 10 in
         match condition digit with
         | false -> accumulator
         | true -> digit :: accumulator)
  in
  digits_list_by_condition' number condition []
;;

digits_list_by_condition 123456789 (fun element -> element mod 2 = 0);;
digits_list_by_condition 123456789 (fun element -> element mod 2 = 1);;

let number_from_list list_of_integers condition =
  let rec number_from_list' list_of_integers condition accumulator =
    match list_of_integers with
    | [] -> accumulator
    | head :: tail ->
      number_from_list' tail condition
        (let add = condition head in
         match add with
         | false -> accumulator
         | true -> head + (accumulator * 10))
  in
  number_from_list' list_of_integers condition 0
;;

number_from_list [ 1; 2; 0] (fun element -> element mod 2 = 0);;

List.filter (fun element -> element mod 2 = 1) [1;2;3];;

let number_from_list2 condition condition2 list_of_integers= 
  List.fold_left (condition) 0 (List.filter condition2 list_of_integers);;

number_from_list2 (fun element element2 -> element*10+element2) (fun element -> element mod 2 = 1) [1;2;3;4;5];;