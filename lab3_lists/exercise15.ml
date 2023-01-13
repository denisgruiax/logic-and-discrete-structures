(*
  15. Încercând să folosească List.fold_right, cineva a scris:
  let what f lst = List.fold_right (fun e r -> f r :: e) lst []
  Interpretorul indică tipul ('a list -> 'a) ->' 'a list list -> 'a list
  a) Explicați de ce funcția what poate fi aplicată doar când lst e o listă de liste.
  b) Simplificând, particularizăm let what1 = what List.length și obținem o funcție care ia o listă de liste de întregi.
  Argumentați (preferabil cu o demonstrație prin inducție) ce face funcția.
*)

(*
  a)

  Things to take in consideration
    List fold right analysis
      How e is the element of the lst that should be of type 'a and r is result where all operations are stored
      Now, list fold right takes a function that takes a element and the result/accumulator.
      How the arguments of function that list fold right takes in are inversed f of "what" waits a a' list,
      f r::e -> from here r will return a type 'a, and how the operand "::" is here, it makes e to be a list,

      So, what take a function that takes a a' list and the result of it is added in a list. 
*)

let rec fold_right f l accu =
  match l with [] -> accu | a :: b -> f a (fold_right f b accu);;

let what f lst = List.fold_right (fun e r -> f r :: e) lst [];;
let what1 lst = what List.length lst;;

List.length [1;2;3;4;5;6;7;8;9];;

what1 [[9;9];[1;2];[1];[3]];;

(*
  a::b -> List.length (fold b) :: [1]
  ->[1;2] ; [1] ; [3]
    <-    <-     <-
*)