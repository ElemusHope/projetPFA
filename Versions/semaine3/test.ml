open Printf

let print_sudoku nomfichier =
  (* read data *)
  let res = Lire.lire nomfichier in

  (* string -> list int *)
  let list_string = Lire.explode res in

  (* list int -> list list int*)
  let list_list = Lire.split 9 list_string in

  let init = Board.of_list list_list in

  Board.print (init)
;;

let print_resulta_sudoku nomfichier =
  (* read data *)
  let res = Lire.lire nomfichier in

  (* string -> list int *)
  let list_string = Lire.explode res in

  (* list int -> list list int*)
  let list_list = Lire.split 9 list_string in

  let init = Board.of_list list_list in

  Board.print (Board.sudoku init)
;;

print_sudoku "grid0.txt";;
print_resulta_sudoku "grid0.txt";;

