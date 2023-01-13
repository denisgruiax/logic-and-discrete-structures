(*3. Aplicarea repetată a unei funcții
  în laboratorul trecut am scris funcții de ordin superior (funcționale) care aplicau o funcție de 2, 3, 4 ori.
  Definiți (recursiv) o funcție care ia ca parametru un întreg n și o funcție, și returnează funcția compusă cu
  ea însăși de n ori.*)

let rec function_composition f x = function
  | 0 -> f x
  | n -> function_composition f (f x) (n - 1)
;;

function_composition (fun x -> x + 1) 0 2;;
