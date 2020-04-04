open Printf;;

(* Graphics.draw_string (Board.get_as_string list_sudoku ((j-1), (s-1))); *)
let init_sudoku nomfichier =
  (* read data *)
  let res = Lire.lire nomfichier in

  (* string -> list int *)
  let list_string = Lire.explode res in

  (* list int -> list list int*)
  let list_list = Lire.split 9 list_string in

  let init = Board.of_list list_list in

  init
;; 

let sudoku_test = init_sudoku "grid0.txt";;
let sudoku_test = Board.with_val sudoku_test (1,1) 1;;
Board.print (sudoku_test);;