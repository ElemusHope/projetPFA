open Graphics;;

Graphics.open_graph " 550x550";;
Graphics.set_window_title "Boyu & Peng SUDOKU ^^";;

let draw_base n =
  for i=1 to 10 do
    Graphics.moveto (n) (50*i);
    Graphics.lineto (n*10) (50*i);
    Graphics.moveto (50*i) (n); 
    Graphics.lineto (50*i) (n*10);
  done;
  for s=1 to 9 do
    for j=1 to 9 do
      Graphics.moveto (j*50+22) (s*50+20);
      Graphics.draw_char '1';
      
    done;
  done
;;


(* Graphics.draw_string (Board.get_as_string list_sudoku ((j-1), (s-1))); *)
(* let print_sudoku nomfichier =
    (* read data *)
    let res = Lire.lire nomfichier in
  
    (* string -> list int *)
    let list_string = Lire.explode res in
  
    (* list int -> list list int*)
    let list_list = Lire.split 9 list_string in
  
    let init = Board.of_list list_list in
  
    draw_base 50 (init)
;; *)


(* print_sudoku "grid0.txt";; *)
draw_base 50;;