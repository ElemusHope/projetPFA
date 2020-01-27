open Graphics;;
exception End;;
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

let go () = skel
  (* 初始化函数 *)
  (fun () -> Graphics.clear_graph (); Graphics.moveto 0 (Graphics.size_y() -12) )

  (* 结束主程序 *)
  (fun () -> Graphics.clear_graph() )
  
  (* 监听到键盘输入时执行的函数 *)
  handle_char

  (* 监听到鼠标点击时执行的函数 *)
  (fun x y -> Graphics.moveto x y)

  (* 检测到异常时执行的函数 *)
  (fun e -> () );;

Graphics.open_graph "";;
go ();;
