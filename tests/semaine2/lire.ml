let lire fichier =
  let ic = open_in fichier in
  let line = input_line ic in
  close_in ic;
  line;;

let explode s =
  let rec exp i l =
    if i < 0 then l 
    else exp (i - 1) ((int_of_char(s.[i]))::l) in 

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





