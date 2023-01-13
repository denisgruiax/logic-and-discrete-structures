(*13. Scrieți o funcție care desparte o listă în două liste a căror lungime diferă cu cel mult 1,
  punând alternativ câte un element în fiecare din liste. (Funcția va returna o pereche de liste).*)

let split_list list =
  let rec split_list' (acc, acc2) count = function
    | [] -> (List.rev acc, List.rev acc2)
    | head :: tail -> (
        let result = count mod 2 in
        match result with
        | 0 -> split_list' (head :: acc, acc2) (count + 1) tail
        | _ -> split_list' (acc, head :: acc2) (count + 1) tail)
  in
  split_list' ([], []) 0 list;;

split_list [1;2;3;4;5;6;7;8;9];;