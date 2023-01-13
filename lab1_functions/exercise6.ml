(*Exercițiul 6: Operații cu funcții
  În matematică, am extins uneori operatorul + de la numere la funcții, definind funcția f + g prin relația (f
  + g)(x) = f(x) + g(x)
  a) Definiți în ML o funcție care ia ca parametru două funcții f și g și returnează funcția definită ca suma lor
  prin relația de mai sus.
  b) Scrieți o funcție mai generală, care primește ca parametru și operatorul binar (o funcție de două
  argumente) care e aplicată (valorilor) celor două funcții. Verificați că o puteți folosi cu operatorul (+) și
  ( * ) pentru a calcula suma și produsul.*)

let function_operations f g x = f x + g x
let function_binary_operations operand f g x = operand (f x) (g x);;

function_operations (fun x -> x + 1) (fun x -> x + 2) 0;;
function_binary_operations ( + ) (fun x -> x + 1) (fun x -> x + 2) 0;;
function_binary_operations ( * ) (fun x -> x + 1) (fun x -> x + 2) 0;;
