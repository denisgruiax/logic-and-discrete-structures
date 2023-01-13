(*Exercițiu: Scrieți o funcție care ia ca parametri trei reali a, b, c și tipărește soluțiile ecuației de gradul doi 
ax2+bx+c=0, sau un mesaj dacă nu există soluții reale. Folosiți funcția predefinită sqrt : float -> float
pentru rădăcina pătrată și nu uitați conversiile de la întreg la real unde sunt necesare. Note de rezolvare: 
Folosiți secvențierea când trebuie tipărite două soluții. Folosiți o variabilă locală pentru delta.*)

let solutions a b c =
  let delta = (b ** 2.) -. (4. *. a *. c) in
  if delta > 0. then (
    Printf.printf "x1 = %f " ((-.b -. sqrt delta) /. 2.);
    Printf.printf "x2 = %f\n" ((-.b +. sqrt delta) /. 2.))
  else if delta = 0. then Printf.printf "x = %f\n" (-.b /. 2.)
  else Printf.printf "No real solutions.\n";;

solutions 1. 2. 1.;;
solutions 1. 4. 3.;;
solutions 1. 1. 2.;;