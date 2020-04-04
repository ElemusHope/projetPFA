open Graphics;;
open Printf;;
exception End;;

(* 创建一个type的类型 *)
type case = { x : int; y : int };;
let case_current = ref {x=0;y=0};;
let chiffre_current = ref 0;;
let x_margin = 300;;
let y_margin = 200;;
let list_decision =  ref [[]];;

(* 
 * 打印数独函数
 *)
let print_sudoku n list_sudoku=
  for i=1 to 10 do
    if i=1 || i=4 || i=7 || i=10 then 
    (Graphics.set_line_width 3;

    Graphics.moveto (x_margin+n) (y_margin+50*i);
    Graphics.lineto (x_margin+n*10) (y_margin+50*i);
    Graphics.moveto (x_margin+50*i) (y_margin+n);
    Graphics.lineto (x_margin+50*i) (y_margin+n*10))
    else 
    (Graphics.set_line_width 1;
    Graphics.moveto (x_margin+n) (y_margin+50*i);
    Graphics.lineto (x_margin+n*10) (y_margin+50*i);
    Graphics.moveto (x_margin+50*i) (y_margin+n);
    Graphics.lineto (x_margin+50*i) (y_margin+n*10))
  done;
  for s=1 to 9 do
    (* 遍历横向坐标 *)
    for j=1 to 9 do
      Graphics.moveto (x_margin+j*50+20) (y_margin+(10-s)*50+25);
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

let sudoku_test = init_sudoku "grid0.txt";;
let sudoku_current := ref sudoku_test;;

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


(* 
 * 在右侧显示当前属于哪个case，之后要改掉
 *)
let afficher_select_case ()=
  Graphics.moveto (1000) (120);
  Graphics.draw_string (string_of_int((!case_current).x));
  Graphics.moveto (1000) (100);
  Graphics.draw_string (string_of_int((!case_current).y));
  Graphics.moveto (1000) (150);
  Graphics.draw_string (string_of_int(!chiffre_current));
  Graphics.set_color 0xbbdefb;
  Graphics.fill_rect ((((!case_current).x)+1)*50+1+x_margin) ((9-((!case_current).y))*50+1+y_margin) 48 48;
  Graphics.set_color black;;

let rec remplir_retour l sudoku_test= 
    match l with 
    | [[]] -> sudoku_test
    | [x;y;v]::s -> let sudoku_current = (Board.with_val sudoku_test (x, y) v) in remplir_retour s sudoku_current
;; 

  let select_reponse x y = 
    if x<600+x_margin || x>840+x_margin || y<260+y_margin || y>500+y_margin then () else (
      let x_current = (((x-600-x_margin)/80)+1) in
      let y_current = (2-((y-260-y_margin)/80)) in
      let chiffre_cur =3*y_current+x_current in
      chiffre_current := chiffre_cur;
      list_decision := Lire.liste_vals (!case_current).x (!case_current).y !chiffre_current;
      sudoku_current := remplir_retour (!list_decision) sudoku_test;;
    );;

(* 
 * 计算当前鼠标点击位置属于哪个case
 *)
let select_case x y= 
  if x<50+x_margin || x>500+x_margin || y<50+y_margin || y>500+y_margin then (select_reponse x y) else (
    let x_current = (((x-x_margin)/50)-1) in 
    let y_current = (9-((y-y_margin)/50)) in
    case_current := {x=x_current;y=y_current};
  );;


let afficher_clavier () =
  for i=1 to 4 do
    Graphics.set_line_width 1;
    Graphics.moveto (x_margin+600) (y_margin+180+80*i);
    Graphics.lineto (x_margin+840) (y_margin+180+80*i);
    Graphics.moveto (x_margin+520+80*i) (y_margin+260);
    Graphics.lineto (x_margin+520+80*i) (y_margin+500);
  done;
  for s=0 to 2 do
    (* 遍历横向坐标 *)
    for j=1 to 3 do
      Graphics.moveto (x_margin+520+j*80+40) (y_margin+420-s*80+40);
      (* Graphics.draw_char '1'; *)
      Graphics.draw_string (string_of_int(3*s+j));
    done;
  done;
;;

(* 
 * 刷新数独
 *)
let refresh_sudoku sudoku_current =
  Graphics.clear_graph ();
  (* Graphics.draw_image (Ig.init_image "img.pbm") 0 0; *)
  afficher_select_case ();
  Graphics.moveto 0 (Graphics.size_y() -12); 
  print_sudoku 50 (sudoku_current);
  afficher_clavier ();
  ;;

let init_sudoku_graphics nom_sudoku =
  refresh_sudoku nom_sudoku;
;;

(* 主程序运行方法，持续监听键盘或鼠标动作，当输入 & 时结束主程序 *)
let go nom_sudoku = skel
  (* 初始化函数 *)
  (fun () -> init_sudoku_graphics nom_sudoku)

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
Graphics.open_graph " 1920x1080";;
Graphics.set_window_title "SB Ghoul Biss";;
go sudoku_test;;
(* Graphics.open_graph " 1920x1080";;
Graphics.draw_image (Ig.init_image "imgGhoul.pbm") 0 0;;
read_line ();;  *)
