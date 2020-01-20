open Printf

let res = Lire.lire "grid0.txt";;
let list_string = Lire.explode res;;
let list_list = Lire.split 9 list_string;;
let init = Board.of_list list_list;;

Board.print (sudoku init);;
(* List.iter (printf "%d") list_string;; *)

