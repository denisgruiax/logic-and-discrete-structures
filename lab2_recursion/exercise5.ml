(*5. Resturi modulo p
  În matematică știm că dacă p e un număr prim, și a nu se divide cu p, atunci șirul a, a2, a3, ... va ajunge la
  1, luând numerele modulo p (adică resturile la împărțirea cu p).
  De exemplu, fie p = 7 și a = 4. Atunci a2 = 16 ≡ 2 (mod 7), și a3 = a2 * a ≡ 2 * 4 ≡ 1 (mod 7).
  (Se spune că mulțimea resturilor nenule modulo p prim formează un grup multiplicativ.)
  Scrieți o funcție care ia ca parametru un număr întreg a și un număr p (presupus prim) și returnează cea
  mai mică putere n pentru care an ≡ 1 mod p (sau returnează 0 dacă a se divide cu p).
  Indicație: scrieți o funcție auxiliară care mai are ca parametri și exponentul k respectiv valoarea ak (mod
  p), și care se apelează recursiv până când ak ≡ 1 (mod p).*)

(*let rec mod_prime number prime accumulator count =
  let left = accumulator mod prime in
  match left with
  | 1 -> count
  | _ -> mod_prime number prime (accumulator mod prime * number) (count + 1)
  ;; *)

let mod_prime number prime = let rec mod_prime' number prime accumulator count = match accumulator mod prime with
    |1 -> count
    |_ -> mod_prime' number prime ((accumulator mod prime)*number) (count+1) in mod_prime' number prime number 1;;

mod_prime 4 7;;
mod_prime 4 7;;
