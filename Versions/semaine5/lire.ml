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

let val_modifier ((x,y),v) l = 
  match l with 
  h::t -> h::((x,y),v) t;;

  
  let rechercher [x;y;v] l = (*liste -> return int position*)
    let rec rechercher_aux [x;y;v] acc l = 
    match l with
      |[[]] -> acc
      |[a;b;c]::s -> if a = x && b = y && c = v then acc
        else rechercher_aux [x;y;v] (1+acc) s
    in rechercher_aux [x;y;v] 0 l;;
    
  let l = [];;
  let rec liste_val [x;y;v] l=
    match l with
    |[[]]->[x;y;v]::[]
    |h::s -> h::([x;y;v]::s);;
  let liste_to_initial [x;y;v] l = 
     drop (rechercher [x;y;v] l) l;; (*[1;2;3] [[1;2;4];[1;2;3];[4;2;3]] to [[1; 2; 3]; [4; 2; 3]] *)

  let rec liste_initial l = 
    match l with 
    |[[]] -> [[]]]
    |[x;y;v]:: s -> [x;y;0]::liste_initial s;;  (*[[1; 2; 0]; [4; 2; 0]] *))
    
    liste_initial (liste_to_initial [1;2;3] [[1;2;4];[1;2;3];[4;2;3]])
liste [1;2;3] [[1;2;4];[1;2;3];[4;2;3]];;
let liste [x;y;v] l = 
  let rec liste_aux l1 l2 =
  l1@l2
  in liste_aux (take (rechercher [x;y;v] l) l) (liste_initial (liste_to_initial [x;y;v] l));;

  take (rechercher [1;2;3] [[1;2;4];[1;2;3];[4;2;3]] ) [[1;2;4];[1;2;3];[4;2;3]];; 

  let rec remplir l = 
    match l with 
    | [[]]-> [[]] 
    | [x;y;v]:: s -> (Board.with_val (b: t) (x, y) v) remplir s