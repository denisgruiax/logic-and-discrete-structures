(*
  4. Pentru tipurile colecție (liste, mulțimi, dicționare) e util să avem funcții care ne spun dacă există un element 
  care satisface o anume condiție, respectiv dacă toate elementele satisfac condiția.
Implementați funcțiile exists și for_all pentru dicționare, folosind fold. 
Ele iau ca parametru o funcție booleană de cheie și valoare (care exprimă condiția) și dicționarul în care 
se face căutarea. (Ele există ca funcții standard, deci puteți vedea tipul lor în documentație).
Încercați să scrieți prelucrarea folosind o excepție pentru a întrerupe parcurgerea dacă răspunsul nu mai depinde de 
restul elementelor (true pentru exists, resp. false pentru for_all). În acest caz puteți folosi mai simplu iter. 
Urmați exemplul cu liste de la curs și din notițe (sec. 5.1).
*)

module Dictionary = Map.Make (String)

exception ExceptionBool of bool

let dictionary_exists dictionary condition =
  try
    Dictionary.iter
      (fun key value -> if condition value then raise Exit)
      dictionary;
    false
  with Exit -> true

let dictionary_for_all dictionary condition =
  try
    Dictionary.iter
      (fun key value -> if not (condition value) then raise Exit)
      dictionary;
    true
  with Exit -> false

let dictionary = Dictionary.(singleton "x" 2 |> add "y" 1 |> add "z" 3);;

dictionary_exists dictionary (fun elt -> elt > 2);;
dictionary_for_all dictionary (fun elt -> elt > 0);;
