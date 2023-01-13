(*Exercițiul 1: minim/maxim (discutat la curs) Scrieți o funcție care returnează minimul/maximul a trei valori
  date ca parametri. Folosiți funcțiile predefinite min respectiv max care funcționează cu orice valori de
  același tip. Remarcați tipul funcției scrise și verificați că funcționează și cu întregi și cu reali (și chiar cu
  șiruri), însă nu cu un amestec.*)

let min_of_three arg arg2 arg3 = min arg (min arg2 arg3)
let max_of_three arg arg2 arg3 = max arg (max arg2 arg3);;

min_of_three 2 1 3;;
min_of_three 2. 1. 0.;;
min_of_three "a" "b" "c";;

max_of_three 2 1 3;;
max_of_three 2. 1. 0.;;
max_of_three "a" "b" "c";;
