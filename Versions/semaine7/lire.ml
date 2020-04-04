let lire fichier =
  let ic = open_in fichier in
  let line = input_line ic in
  close_in ic;
  line;;

let explode s =
  let rec exp i l =
    if i < 0 then l 
    else exp (i - 1) ((int_of_char(s.[i])-48)::l) in 

  exp (String.length s-1) [];;


let rec take n l =
  if n = 0 then [] else
    match l with 
      h::t -> h:: take (n-1) t
;;

let rec drop n l =
  if n=0 then l else
    match l with
      h::t -> drop (n-1) t
;;

   
let rec split chunksize l=
  try 
    take chunksize l::
    split chunksize (drop chunksize l)
  with
    _ -> match l with [] -> [] | _ -> [l]
;;
  
  let rechercher [x;y;v] l = (*liste -> return int position*)
    let rec rechercher_aux [x;y;v] acc l = 
    match l with
      |[[]] -> acc
      |[a;b;c]::s -> if a = x && b = y && c = v then acc
        else rechercher_aux [x;y;v] (1+acc) s
    in rechercher_aux [x;y;v] 0 l;;
    
  (* let rec liste_val x y v l=  (*插入参数*)
    match l with
    |[[]]->[[x;y;v]]
    |h::s -> [x;y;v]::(liste_val x y v s);; *)
  let l = ref [[0;0;0]];;
  let liste_val x y v = (*所有添加的参数存入liste*)
    l:= [x;y;v]::!l;
    !l;; 

    let rec liste_v l= take (rechercher [0;0;0] l) l;; (*删除[0;0;0]*)
    let rec rev_append l1 l2 = (*倒序函数*)
      match l1 with 
      |[]-> l2
      |x::s -> rev_append s (x :: l2);;
    
    let rec liste_vals x y v=  (*倒序liste*)
      rev_append (liste_v (liste_val x y v)) [];;

  let liste_to_initial [x;y;v] l = 
     drop (rechercher [x;y;v] l) l;; (*[1;2;3] [[1;2;4];[1;2;3];[4;2;3]] to [[1; 2; 3]; [4; 2; 3]] *)

  let rec liste_initial l = 
    match l with 
    |[[]] -> [[]]
    |[x;y;v]:: s -> [x;y;0]::liste_initial s;;  (*[[1; 2; 0]; [4; 2; 0]] *)
    
    (*liste_initial (liste_to_initial [1;2;3] [[1;2;4];[1;2;3];[4;2;3]])
    liste [1;2;3] [[1;2;4];[1;2;3];[4;2;3]];;*)
let liste [x;y;v] l = (*重组liste 未更改部分 和 更改部分 结合 成新的liste*)
  let rec liste_aux l1 l2 =
  l1@l2
  in liste_aux (take (rechercher [x;y;v] l) l) (liste_initial (liste_to_initial [x;y;v] l));;

  (*take (rechercher [1;2;3] [[1;2;4];[1;2;3];[4;2;3]] ) [[1;2;4];[1;2;3];[4;2;3]]*)