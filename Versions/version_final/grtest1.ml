open Graphics;;
open Printf;;
exception End;;

(* creer type case *)
type case = { x : int; y : int };;
let case_current = ref {x=0;y=0};;
let nombre_false_existe = ref 0;;
let chiffre_current = ref 0;;
let x_margin = 300;;
let y_margin = 200;;
let list_decision =  ref [[]];;
let jeuTermine = ref false;;
let jeuBeginned = ref false;;
let countSaveTimes = ref 1;;
let nomSudoku = ref "";;

(* 
 * print sudoku currant
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
    (*  *)
    for j=1 to 9 do
      Graphics.moveto (x_margin+j*50+20) (y_margin+(10-s)*50+25);
      (* Graphics.draw_char '1'; *)
      Graphics.draw_string (Board.get_as_string list_sudoku ((j-1), (s-1)));
    done;
  done;
;;

(* le jeu indique si vous avez gagné ou perdu *)
let gagne () = 
  jeuTermine := true;
  Graphics.clear_graph ();
  Graphics.draw_image (Ig.init_image "victory.pbm") 0 0;
;;

let perdu () = 
  jeuTermine := true;
  Graphics.clear_graph ();
  Graphics.draw_image (Ig.init_image "defeat.pbm") 0 0;
;;


(* 
 * Initialiez sudoku de nom fichier
 *)
let init_sudoku nomfichier =
    (* read data *)
    let res = Lire.lire nomfichier in
  
    (* string -> list int *)
    let list_string = Lire.explode res in
  
    (* list int -> list list int*)
    let list_list = Lire.split 9 list_string in
  
    let init = Board.of_list list_list in

    nomSudoku := nomfichier;
  
    init
;; 

let sudoku_grid = init_sudoku "grid0.txt";;
let sudoku_solution = init_sudoku "solution0.txt";;
let sudoku_init = init_sudoku "grid0.txt";;
let sudoku_current = ref sudoku_grid;;

(* enregistrer et charger une partie *)
let enregistrerJeu ()=
  let file = String.concat "_JeuEnregistre" [(string_of_int (!countSaveTimes));".txt"] in
  let tmp = (!countSaveTimes)+1 in
  countSaveTimes := tmp;
  let message = (Lire.lstring_string (!sudoku_current)) in
    let oc = open_out file in    
    fprintf oc "%s\n" message;   
    close_out oc;                
;;


(* Verifier *)
let rec check list=
  match list with 
  |[]->[]
  |x::s-> print_int x; check s
;;

(* main de jeu *)
let skel f_init f_end f_key f_mouse f_except =
  (* Initialiser le jeu *)
  f_init ();

  try
  (* boucle for a ecoute des events  *)
    while true do
      try
        let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed]
      in 
        (* clavier ecoute *)
        if s.Graphics.keypressed then f_key s.Graphics.key

        (* souris ecoute *)
        else if s.Graphics.button then f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
    with
        End -> raise End
      | e -> f_except e
    done
  with
    End -> f_end ()
;;

let relanceJeu nom_fichier=
  let sudoku_tmp = init_sudoku nom_fichier in
  sudoku_current := sudoku_tmp;
  case_current := {x=0;y=0};
  chiffre_current := 0;
  list_decision := [[]];
  jeuTermine := false;
;;

let select_false_cases () =
  for i=0 to 8 do
    for j=0 to 8 do
      let chiffre_current_tmp = (Board.get (!sudoku_current) (i,j)) in
      let chiffre_solution = (Board.get sudoku_solution (i,j)) in
      if chiffre_current_tmp=chiffre_solution then() else(
        if ((Board.get (!sudoku_current) (i,j)) = 0) then() else(
          nombre_false_existe := 0;
          let tmp = (!nombre_false_existe) in
          nombre_false_existe := tmp+1;
          Graphics.set_color 0xff0000;
          Graphics.fill_rect ((i+1)*50+1+x_margin) ((9-j)*50+1+y_margin) 48 48;
          Graphics.set_color black;
        );
      );
    done;
  done;
;;



(* 
 * affichier des elements importants
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
  Graphics.set_color black;
  if (!nombre_false_existe)=0 then() else(
    select_false_cases ();
  )
;;

let afficher_clavier () =
  for i=1 to 4 do
    Graphics.set_line_width 1;
    Graphics.moveto (x_margin+600) (y_margin+180+80*i);
    Graphics.lineto (x_margin+840) (y_margin+180+80*i);
    Graphics.moveto (x_margin+520+80*i) (y_margin+260);
    Graphics.lineto (x_margin+520+80*i) (y_margin+500);
  done;
  for s=0 to 2 do
    for j=1 to 3 do
      Graphics.moveto (x_margin+520+j*80+40) (y_margin+420-s*80+40);
      (* Graphics.draw_char '1'; *)
      Graphics.draw_string (string_of_int(3*s+j));
    done;
  done;
;;

let affichier_buttons () = 
  for i=1 to 3 do
    Graphics.set_line_width 1;
    (* de gauche a droite *)
    Graphics.moveto (x_margin+600) (y_margin+50+75*(i-1));
    Graphics.lineto (x_margin+840) (y_margin+50+75*(i-1));
    (* de haut a bas *)
    Graphics.moveto (x_margin+480+120*i) (y_margin+50);
    Graphics.lineto (x_margin+480+120*i) (y_margin+200);
  done;

  for j=1 to 3 do
    Graphics.set_line_width 1;
    (* de gauche a droite *)
    Graphics.moveto (x_margin+50) (y_margin+530+50*(j-1));
    Graphics.lineto (x_margin+500) (y_margin+530+50*(j-1));
  done;
  for s=0 to 3 do
    Graphics.set_line_width 1;
    (* de haut a bas *)
    Graphics.moveto (x_margin+50+150*s) (y_margin+530);
    Graphics.lineto (x_margin+50+150*s) (y_margin+630);
  done;
  Graphics.moveto (x_margin+660) (y_margin+87);
  Graphics.draw_string "Tip";
  Graphics.moveto (x_margin+780) (y_margin+87);
  Graphics.draw_string "Undo";
  Graphics.moveto (x_margin+780) (y_margin+162);
  Graphics.draw_string "Delete";
  Graphics.moveto (x_margin+660) (y_margin+162);
  Graphics.draw_string "Back";

  Graphics.moveto (x_margin+70) (y_margin+605);
  Graphics.draw_string "Verifier cette case";
  Graphics.moveto (x_margin+220) (y_margin+605);
  Graphics.draw_string "Verifier toutes cases";
  Graphics.moveto (x_margin+370) (y_margin+605);
  Graphics.draw_string "Save";
  Graphics.moveto (x_margin+70) (y_margin+555);
  Graphics.draw_string "Load last save";
  Graphics.moveto (x_margin+220) (y_margin+555);
  Graphics.draw_string "Quit";
  Graphics.moveto (x_margin+370) (y_margin+555);
  Graphics.draw_string "Restart";
  ;;

(* 
 * refresh sudoku
 *)
let refresh_sudoku () =
  if (!jeuBeginned ) then (
    Graphics.clear_graph ();
    (* Graphics.draw_image (Ig.init_image "img.pbm") 0 0; *)
    afficher_select_case ();
    Graphics.moveto 0 (Graphics.size_y() -12); 
    print_sudoku 50 (!sudoku_current);
    afficher_clavier ();
    affichier_buttons ();
    if(Board.comparer_liste (!sudoku_current) sudoku_solution) then(
      gagne();
    ) else(
      if(Lire.countzeros (Array.to_list(!sudoku_current))=0) then (
        perdu();
      )else()
    )
  )else(
    Graphics.draw_image (Ig.init_image "main.pbm") 0 0;
  )
;;


let rec remplir_retour l sudoku_test= 
  match l with 
  | [[]] -> ()
  | [x;y;v]::s -> let sudoku_current = (Board.with_val sudoku_test (x, y) v) in remplir_retour s sudoku_current
;; 

let undo () =
  let x_tmp = List.nth (Lire.liste_fin (!list_decision)) 0 in
    let y_tmp = List.nth (Lire.liste_fin (!list_decision)) 1 in
    let sudoku_tmp = Board.with_val (!sudoku_current) (x_tmp, y_tmp) 0 in
    sudoku_current := sudoku_tmp;
    let list_decision_tmp =  Lire.liste_take_fin (!list_decision) in
    list_decision := list_decision_tmp;
  ;;

  let rec do_liste liste_verse =
    match liste_verse with 
    |[]-> ()
    |x::s -> (let x_tmp = List.nth (Lire.liste_fin liste_verse) 0 in
             let y_tmp = List.nth (Lire.liste_fin liste_verse) 1 in
             let value_tmp = List.nth (Lire.liste_fin liste_verse) 2 in
             let sudoku_tmp = Board.with_val (!sudoku_current) (x_tmp, y_tmp) value_tmp in
             sudoku_current := sudoku_tmp;
             do_liste s;
             ) 
  ;;

  (* let liste_val x y v = (*所有添加的参数存入liste*)
    (!list_decision) == [x;y;v]::(!list_decision);
    (!list_decision);
  ;; 
  let rec liste_v l= Lire.take (Lire.rechercher [0;0;0] l) l;; (*删除[0;0;0]*)
  let rec rev_append l1 l2 = (*倒序函数*)
    match l1 with 
    |[]-> l2
    |x::s -> rev_append s (x :: l2)
    ;;
  
  let rec liste_vals x y v=  (*倒序liste*)
    rev_append (liste_v (liste_val x y v)) []
    ;; *)

  let select_function x y =
    if x<50+x_margin || x>500+x_margin || y<530+y_margin || y>630+y_margin then () else(
      (* la premiere case de haut; function: Verifier ce case*)
      if x>50+x_margin && x<200+x_margin && y>580+y_margin && y<630+y_margin then (
        if (!chiffre_current) = (Board.get sudoku_solution ((!case_current).x,(!case_current).y)) then() else(
          Graphics.moveto (500) (250);
          Graphics.draw_string ("Ce case n'est pas correcte");
        );
      ) else();
  
      (* la deuxieme case de haut; function: Verifier touts cases *)
      if x>200+x_margin && x<350+x_margin && y>580+y_margin && y<630+y_margin then (
        select_false_cases ();
      ) else();

      (* la troiseme case de haut; function: Save *)
      if x>350+x_margin && x<500+x_margin && y>580+y_margin && y<630+y_margin then (
        enregistrerJeu();
      ) else();
  
      (* la premiere case de bas; function: Load last save *)
      if x>50+x_margin && x<200+x_margin && y>530+y_margin && y<580+y_margin then (
        let tmp = String.concat "_JeuEnregistre" [(string_of_int ((!countSaveTimes)-1));".txt"] in
        relanceJeu tmp;
      ) else();
  
      (* la deuxieme case de bas; function: Quit *)
      if x>200+x_margin && x<350+x_margin && y>530+y_margin && y<580+y_margin then (
        raise End;
      ) else();

      (* la troiseme case de bas; function: Restart *)
      if x>350+x_margin && x<500+x_margin && y>530+y_margin && y<580+y_margin then (
        sudoku_current := sudoku_init;
      ) else();
    );;


(* 
 * des botton de jeu
 * si il n'est pas dans le botton, on lance select_function
 *)
let select_buttons x y =
  if x<600+x_margin || x>840+x_margin || y<50+y_margin || y>200+y_margin then (select_function x y) else(
    (* la premiere case de haut; function: Back *)
    if x>600+x_margin && x<720+x_margin && y>125+y_margin && y<200+y_margin then (
      let list_decision_tmp =  Lire.liste_undo [(!case_current).x;(!case_current).y;(!chiffre_current)] (!list_decision) in
      list_decision := list_decision_tmp;
      sudoku_current := sudoku_init;
      let liste_verse = Lire.invertir_liste (!list_decision) in
      do_liste liste_verse;
    ) else();

    (* la deuxieme case de haut; function: Delete *)
    if x>720+x_margin && x<840+x_margin && y>125+y_margin && y<200+y_margin then (
      if ((Board.get sudoku_init ((!case_current).x,(!case_current).y)) = 0) then(
        let sudoku_tmp = Board.with_val (!sudoku_current) ((!case_current).x, (!case_current).y) 0 in
        sudoku_current := sudoku_tmp;
      ) else();
    ) else();

    (* la permiere case de bas; function: Tips *)
    if x>600+x_margin && x<720+x_margin && y>50+y_margin && y<125+y_margin then (
      chiffre_current := (Board.get sudoku_solution ((!case_current).x,(!case_current).y));
      let sudoku_tmp = Board.with_val (!sudoku_current) ((!case_current).x, (!case_current).y) !chiffre_current in
      sudoku_current := sudoku_tmp;
    ) else();

    (* la deuxieme case de bas; function: Undo *)
    if x>720+x_margin && x<840+x_margin && y>50+y_margin && y<125+y_margin then (
      undo ();
    ) else();
  );;

(* 
 * mettre le chiffre choisi dans le sudoku
 * si il n'est pas dans la clavier, on lance select_buttons
 *)
let select_reponse x y = 
  if x<600+x_margin || x>840+x_margin || y<260+y_margin || y>500+y_margin then (select_buttons x y) else(
    let x_current = (((x-600-x_margin)/80)+1) in
    let y_current = (2-((y-260-y_margin)/80)) in
    let chiffre_cur =3*y_current+x_current in
        (* on ne doit pas pouvoir cliquer et modifier une case préremplie *)
    if ((Board.get sudoku_init ((!case_current).x,(!case_current).y)) = 0) then(
      chiffre_current := chiffre_cur;
      list_decision := Lire.liste_vals (!case_current).x (!case_current).y !chiffre_current;
      (* remplir_retour (!list_decision) sudoku_test; *)
      let sudoku_tmp = Board.with_val (!sudoku_current) ((!case_current).x, (!case_current).y) !chiffre_current in
      sudoku_current := sudoku_tmp;
    ) else();
  );;



(* 
 * calculer la position de souris est à quelle case
 * si il n'est pas dans la case, on lance select_reponse
 *)
let select_case x y= 
  (* si le jeu fini *)
  if (!jeuTermine) then (
    raise End;
  )else();

  (* si pas encore start *)
  if (!jeuBeginned) then () else(
    jeuBeginned := true;
  );
  if x<50+x_margin || x>500+x_margin || y<50+y_margin || y>500+y_margin then (select_reponse x y) else (
    let x_current = (((x-x_margin)/50)-1) in 
    let y_current = (9-((y-y_margin)/50)) in
    case_current := {x=x_current;y=y_current};
    chiffre_current := (Board.get (!sudoku_current) (x_current,y_current));
  );;

(* go - main de jeu *)
let go () = skel
  (* Initialiser le programme*)
  (fun () -> refresh_sudoku();enregistrerJeu())

  (* finir le programme *)
  (fun () -> Graphics.clear_graph())
  
  (* lorsque clavier *)
  (fun c -> () )

  (* lorsque souris *)
  (fun x y -> Graphics.moveto x y; select_case x y; refresh_sudoku();)

  (* lorsque exception *)
  (fun e -> () )
;;


(* lance le programme *)
Graphics.open_graph " 1920x1080";;
Graphics.set_window_title "SUDOKU";;
go ();; 