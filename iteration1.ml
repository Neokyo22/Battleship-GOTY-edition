#use "CPgraphics.ml" ;;

(*
Type structuré qui contient les paramètres du jeu, c'est à dire la taille de la marge autour des grilles, et l'espace entre les 2 grilles, 
en pixels (margin), la largeur d'un cellule en pixels (cell_size) la hauteur de la zone d’affichage des messages de consigne au joueur, 
en pixels (message_size) et la taille des grilles (largeur d'une grille) en nombre de case (grid_size).
-> Des modifications, notamments des ajouts seront potentiellement effectués sur ce type. Il faudra donc modifier la fonction init_params en conséquence.
*)
type t_params = {margin : int ; cell_size : int ; message_size : int ; grid_size : int} ;;


(*
Prend en paramètre 4 entiers correspondants aux paramètres du jeu voulus (taille de la marge, des cellules,
de la zone d'affichage des messages de consignes et des grilles) et renvoie un type t_params avec les valeurs entrées.
Renvoie une erreur quand au moins une des valeurs est negative ou nulle.
Exemple d'exécution :
# init_params (30, 15, 60, 10) ;;
- : t_params = {margin = 30; cell_size = 15; message_size = 60; grid_size = 10}
*)
let init_params (p_margin, p_cell_size, p_message_size, p_grid_size : int * int * int * int) : t_params =
  if p_margin <= 0 || p_cell_size <= 0 || p_message_size <= 0 || p_grid_size <= 0 then
    failwith "Les valeurs des paramètres sont non conformes"
  else
    {margin = p_margin ; cell_size = p_cell_size ; message_size = p_message_size ; grid_size = p_grid_size}
;;


(*
Prend en paramètres une position p_pos (int*int) et un t_params, et affiche une grille vide avec ses coordonnées à la position voulue,
selon les valeurs des paramètres du jeu.
Ne renvoie rien.
*)
let display_grid(p_pos, p_params : (int * int) * t_params) : unit =
  draw_rect(fst(p_pos) + p_params.cell_size, snd(p_pos) + p_params.message_size , p_params.cell_size * p_params.grid_size, p_params.cell_size * p_params.grid_size);
  for i = 0 to 8 do
    draw_rect((fst(p_pos) + (p_params.cell_size * (i + 2))), (snd(p_pos) + p_params.message_size), 1, (p_params.cell_size * p_params.grid_size));
    draw_rect((fst(p_pos) + p_params.cell_size), (snd(p_pos) + p_params.message_size + (p_params.cell_size * (i + 1))), (p_params.cell_size * p_params.grid_size), 1)
  done ;
  for k = 0 to 1 do
    for i = 1 to p_params.grid_size do
      moveto (p_params.margin + k * (p_params.margin + (p_params.grid_size + 1) * p_params.cell_size), p_params.margin + p_params.message_size + 10 * p_params.cell_size - i * p_params.cell_size) ;
      draw_string (string_of_int (i))
    done ;
    for j = 1 to p_params.grid_size do
      moveto (p_params.margin + p_params.cell_size + k * (p_params.margin + (p_params.grid_size + 1) * p_params.cell_size) + (j - 1) * p_params.cell_size, p_params.margin + p_params.message_size + p_params.grid_size * p_params.cell_size) ;
      draw_char (char_of_int(int_of_char('A') + j - 1))
    done
  done
;;



(*
Prend en paramètre un t_params, et affiche dans la fenêtre graphique les 2 grilles de jeu vides, avec les coordonnées et le nom des joueurs.
Ne renvoie rien.
*)
let display_empty_grid (p_params : t_params) : unit =
  let pos_grid1 : int * int = p_params.margin, p_params.margin
  and pos_grid2 : int * int = 2 * p_params.margin + (p_params.grid_size + 1) * p_params.cell_size, p_params.margin
  in
  (
    display_grid (pos_grid1, p_params) ;
    display_grid (pos_grid2, p_params) ;
  ) ;
  moveto (p_params.margin + p_params.cell_size, p_params.margin + p_params.message_size + (p_params.grid_size + 2) * p_params.cell_size) ;
  draw_string ("Ordinateur") ;
  moveto (2 * p_params.margin + (p_params.grid_size + 2) * p_params.cell_size, p_params.margin + p_params.message_size + (p_params.grid_size + 2) * p_params.cell_size) ;
  draw_string ("Joueur")
;;


(*
Fonction d'affichage.
Ne prend rien en paramètre, initialise un t_params et ouvre la fenêtre graphique aux dimensions appropriées , et met à jour son titre.
Cette fonction ne renvoie rien.
*)
let battleship_game () : unit =
  let params : t_params = init_params (30, 15, 60, 10)
  in
  (
    open_graph (3 * params.margin + 2 * (params.grid_size + 1) * params.cell_size, 2 * params.margin + params.message_size +(params.grid_size + 2) * params.cell_size) ;
    display_empty_grid(params);
    set_window_title ("Battleship GOTY edition")
  )
;;