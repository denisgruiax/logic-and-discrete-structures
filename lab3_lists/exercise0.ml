[];;

let rec reverse_digits_list = function
  | number when number < 10 -> [ number ]
  | number -> (number mod 10) :: reverse_digits_list (number / 10)
;;

reverse_digits_list 3

let rec sum_list = function [] -> 0 | head :: tail -> head + sum_list tail;;

sum_list [];;
sum_list [ 1; 2; 3; 4; 5 ]

let rec first_two_equal = function
  | element1 :: element2 :: _ -> element1 = element2
  | _ -> false
;;

first_two_equal [];;
first_two_equal [ 1; 2; 3 ];;
first_two_equal [ 1; 1; 2 ]

let rec exist element = function
  | [] -> false
  | head :: tail -> element = head || exist element tail
;;

exist 5 [ 1; 2; 3 ];;
exist 4 [ 1; 5; 4; 9 ]

let exist2 element =
  let rec exist2' element result = function
    | [] -> result
    | head :: tail -> exist2' element (element = head || result) tail
  in
  exist2' element false
;;

exist2 5 [ 1; 2; 5; 6 ]

let length =
  let rec length' count = function
    | [] -> count
    | head :: tail -> length' (count + 1) tail
  in
  length' 0
;;

length [ 1; 2; 3; 4; 5 ];;
List.iter print_int [ 1; 2; 3 ];;
List.map (fun element -> element * element) [ 1; 2; 3 ];;

let rec iter_list function' = function
  | [] -> ()
  | head :: tail ->
    function' head;
    iter_list function' tail
;;

iter_list print_int [ 1; 2; 3 ];;

List.fold_left ( * ) 1 [1;2;3];;

4 |> print_int;; (*4 is input for function*) (*function takes input the 4*)