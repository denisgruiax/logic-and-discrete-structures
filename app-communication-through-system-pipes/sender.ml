Printf.printf "L0";;

let file_descriptor = Unix.openfile "pipe" [O_WRONLY; O_TRUNC; O_CREAT] 0o666;;

Printf.printf "L1";;

let buf = Bytes.of_string "Denis";;

Unix.write file_descriptor buf 0 5;;

Unix.close file_descriptor;;

Printf.printf "L3";;