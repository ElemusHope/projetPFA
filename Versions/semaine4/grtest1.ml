open Graphics;;
open Printf;;
exception End;;

type case = { x : int; y : int };;
let case_current = ref {x=(-1);y=0};;
let print_sudoku n list_sudoku=
  for i=1 to 10 do
    if i=1 || i=4 || i=7 || i=10 then 
    (Graphics.set_line_width 3;
    Graphics.moveto (n) (50*i);
    Graphics.lineto (n*10) (50*i);
    Graphics.moveto (50*i) (n);
    Graphics.lineto (50*i) (n*10))
    else 
    (Graphics.set_line_width 1;
    Graphics.moveto (n) (50*i);
    Graphics.lineto (n*10) (50*i);
    Graphics.moveto (50*i) (n);
    Graphics.lineto (50*i) (n*10))
  done;
  for s=1 to 9 do

    (* 遍历横向坐标 *)
    for j=1 to 9 do
      Graphics.moveto (j*50+22) ((10-s)*50+20);
      (* Graphics.draw_char '1'; *)
      Graphics.draw_string (Board.get_as_string list_sudoku ((j-1), (s-1)));
    done;
  done
;;


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


(* let sudoku_test = init_sudoku "grid0.txt";;
(* 测试插入新的数值 *)
print_sudoku 50 sudoku_test;; *)
(* (Board.with_val sudoku_test (0,0) 1);; *)

(* 测试此处是否可用 *)
let rec check list=
  match list with 
  |[]->[]
  |x::s-> print_int x; check s
  ;;


(* check (Board.available sudoku_test (1,0)); *)


(* 主函数 *)
let skel f_init f_end f_key f_mouse f_except =
  (* 初始化函数 *)
  f_init ();

  try
  (* 主程序循环，监听事件发生 *)
    while true do
      try
        let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed]
      in 
        (* 监听到键盘输入 *)
        if s.Graphics.keypressed then f_key s.Graphics.key

        (* 监听到鼠标点击 *)
        else if s.Graphics.button then f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
    with
        End -> raise End
      | e -> f_except e
    done
  with
    End -> f_end ()
;;

let next_line () =
  let (x,y) = Graphics.current_point()
  in if y>12 then Graphics.moveto 0 (y-12)
  else Graphics.moveto 0 y
;;

let handle_char c = match c with
  '&' -> raise End
| '\n' -> next_line ()
| '\r' -> next_line ()
| _ -> Graphics.draw_char c;;

let afficher_select_case ()=
  Graphics.moveto (600) (120);
  Graphics.draw_string (string_of_int((!case_current).x));
  Graphics.moveto (600) (100);
  Graphics.draw_string (string_of_int((!case_current).y));
  Graphics.set_color 0xbbdefb;
  Graphics.fill_rect ((((!case_current).x)+1)*50+1) ((9-((!case_current).y))*50+1) 48 48;
  Graphics.set_color black;;


let select_case x y = 
  if x<50 || x>500 || y<50 || y>500 then () else (
    let x_current = ((x/50)-1) in 
    let y_current = (9-(y/50)) in
    case_current := {x=x_current;y=y_current};
  );;


let refresh_sudoku sudoku_current =
  Graphics.clear_graph ();
  afficher_select_case ();
  Graphics.moveto 0 (Graphics.size_y() -12); 
  print_sudoku 50 (sudoku_current);;

(* 主程序运行方法，持续监听键盘或鼠标动作，当输入 & 时结束主程序 *)
let go nom_sudoku = skel
  (* 初始化函数 *)
  (fun () -> refresh_sudoku nom_sudoku)

  (* 结束主程序 *)
  (fun () -> Graphics.clear_graph() )
  
  (* 监听到键盘输入时执行的函数 *)
  handle_char

  (* 监听到鼠标点击时执行的函数 *)
  (fun x y -> Graphics.moveto x y; select_case x y; refresh_sudoku nom_sudoku;)

  (* 检测到异常时执行的函数 *)
  (fun e -> () )
;;



(* 主程序开始 *)
let sudoku_test = init_sudoku "grid0.txt";;
Graphics.open_graph " 700x550";;
Graphics.set_window_title "SB Ghoul Biss";;
go sudoku_test;;