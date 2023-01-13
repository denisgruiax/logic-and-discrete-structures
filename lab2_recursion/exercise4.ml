(*4. Cifrele unui număr
  Un număr e reprezentat uzual în scris ca un șir de cifre în baza 10.
  Un șir e o noțiune recursivă (un element, sau un șir urmat de un element).
  Putem spune atunci că un număr n e fie o singură cifră, fie ultima cifră (n mod 10) precedată de alt număr
  (n / 10).
  Folosind această definiție scrieți funcții recursive care calculează: suma cifrelor unui număr, numărul de
  cifre, produsul lor, cifra maximă / minimă, etc. *)

let rec digit_sum sum = function
  | 0 -> sum
  | number -> digit_sum (sum + (number mod 10)) (number / 10);;

digit_sum 0 123450;;

let rec digit_number sum = function
  | 0 -> sum
  | number -> digit_number (sum + 1) (number / 10);;

digit_number 0 123450;;

let max_digit = let rec max_digit' digit = function
    |0->digit
    |number -> max_digit' (if digit > number mod 10 then digit else number mod 10) (number/10) in max_digit' 0;;

max_digit 10312;;

let min_digit = let rec min_digit' digit = function
    |0->digit
    |number -> min_digit' (if digit < number mod 10 then digit else number mod 10) (number/10) in min_digit' 9;;

min_digit 9642457;;

