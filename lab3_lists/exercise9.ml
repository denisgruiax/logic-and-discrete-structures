(*9. Scrieți o funcție care ia o listă de cifre și returnează valoarea numărului cu cifrele respective.*)

let list_to_number = let rec list_to_number' accumulator = function
    | [] -> accumulator
    | head :: tail -> list_to_number' (accumulator*10+head) tail in list_to_number' 0;;

list_to_number [ 1; 2; 0 ];;