(*Exercițiu 7: Tarife cu bonus
  În New York, o călătorie cu transportul în comun costă $2.75. La încărcarea cardului de transport se
  acceptă doar sume în multipli de 5 cenți. Pentru încărcarea cu valoarea a cel puțin două călătorii se oferă
  un bonus de 5%, rotunjit la cent. Scrieți o funcție care calculează și returnează suma minimă care trebuie
  încărcată pentru N călătorii, și afișează restul care rămâne pe card. Puteți verifica rezultatele aici*)

let balance = 17.;;

let the_money_needed_for_travel rides =
  let money_needed = rides *. 2.75 in
  if money_needed > balance then
    Printf.printf "Add %f\n" (money_needed -. balance)
  else Printf.printf "Leftover %f\n" (balance -. money_needed)
;;

the_money_needed_for_travel 6.;;
the_money_needed_for_travel 7.;;
the_money_needed_for_travel 8.;;
the_money_needed_for_travel 9.;;
the_money_needed_for_travel 10.;;
the_money_needed_for_travel 11.;;
