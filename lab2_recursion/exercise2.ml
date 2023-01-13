(*2. Cel mai mare divizor comun 
  Știind că cmmdc(a, b) = cmmdc(b, a mod b) dacă b ≠ 0, scrieți o funcție recursivă pentru cel mai mare
  divizor comun. Care e cazul de bază ?*)

let rec cmmdc a = function
  |0 -> a
  |b -> cmmdc b (a mod b);;

cmmdc 4 2;;
