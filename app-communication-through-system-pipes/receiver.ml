Printf.printf "L0";;

let file_descriptor = Unix.openfile "pipe" [O_RDONLY] 0;;

Printf.printf "L1";;

let buf = Bytes.create 10;;

Unix.read file_descriptor buf 1 5;;
Unix.socket
print_bytes buf;;

Unix.close file_descriptor;;

Printf.printf "L3";;