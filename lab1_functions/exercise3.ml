(*Exercițiul 3: an bisect Scrieți o funcție care determină dacă un an (întreg) dat ca parametru e bisect,
  returnând un boolean. Dacă un an e bisect sau nu se poate determina după următoarele reguli (va trebui
  sa le reformulați sau reordonați pentru a scrie funcția):
  a) un an divizibil la 4 e bisect, altfel nu
  b) prin excepție de la a), anii divizibili cu 100 nu sunt bisecți
  c) prin excepție de la b), anii divizibili cu 400 sunt bisecți
*)

let bissextile_year year = (year mod 4 = 0) && (year mod 100 <> 0 || year mod 400 = 0);;

bissextile_year 2000;;
