(*4. Folosind funcțiile din modulul Random,
  scrieți o funcție care construiește o listă de n întregi aleatori, fiecare între 0 și b - 1.*)

let random_list_of_integers size limit =
  if limit > int_of_float (2. ** 30.) then failwith "Value exceeds the limit!"
  else
    let rec random_list_of_integers' size limit accumulator =
      match size with
      | 0 -> Random.full_int limit :: accumulator
      | _ ->
        random_list_of_integers' (size - 1) limit
          (Random.full_int limit :: accumulator)
    in
    random_list_of_integers' size limit [];;

random_list_of_integers 10 2048;;
