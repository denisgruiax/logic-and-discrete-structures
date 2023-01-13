(*Exercițiul 4: Valori distincte 
  Scrieți o funcție cu trei parametri (de același tip oarecare), care returnează câte valori distincte există între 
  argumentele primite (unul, două sau trei) și tipărește, după caz, un mesaj: "toate argumentele sunt 
  distincte/egale" sau "argumentele 1 și 2 (resp. 2 și 3, sau 1 și 3) sunt egale". Evitați pe cât posibil duplicarea 
  de cod: pentru porțiuni de cod similare, creați (și apelați) o funcție care conține partea comună și are ca 
  parametri valorile care diferă.*)

let compare arg arg2 = (arg = arg2) ;;
let distinct_values arg arg2 arg3 = if (compare arg arg2 && compare arg2 arg3) then "All are equal\n"
  else if compare arg arg2 then "Arguments 1 and 2 are equal\n"
  else if compare arg arg3 then "Arguments 1 and 3 are equal\n"
  else if compare arg2 arg3 then "Arguments 2 and 3 are equal\n"
  else "All are distinct\n";;

distinct_values 3 3 3;;
distinct_values 1 1 3;;
distinct_values 1 3 1;;
distinct_values 1 3 3;;
distinct_values 1 2 3;;