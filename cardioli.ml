open Graphics

let () = open_graph " 300x200"

let rec draw_cardioide i = 
  if i <= 200 then
    begin
      let th = atan 1. *. float i /. 25. in
      let r = 50. *. (1. -. sin th) in
      let x = truncate (r *. cos th) in
      let y = truncate (r *. sin th) in
      lineto (150 + x) (150 + y);
      draw_cardioide (i+1)
    end

let () = 
  moveto 200 150;
  draw_cardioide 0;
  ignore(read_key ())