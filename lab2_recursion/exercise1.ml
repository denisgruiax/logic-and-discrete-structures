(*1. Factorial
  Scrieți o funcție care să calculeze n! = 1 * 2 * 3 * ...* (n - 1) * n*)

let rec factorial = function
  |0 -> 1
  |n -> n * factorial (n-1);;

factorial 0;;
factorial 5;;
factorial 3;;