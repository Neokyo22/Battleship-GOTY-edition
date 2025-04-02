(**
Type structuré qui contient les paramètres du jeu, c'est à dire la taille de la marge autour des grilles, et l'espace entre les 2 grilles, 
en pixels (margin), la largeur d'un cellule en pixels (cell_size) la hauteur de la zone d’affichage des messages de consigne au joueur, 
en pixels (message_size) et la taille des grilles (largeur d'une grille) en nombre de case (grid_size).
-> Des modifications, notamments des ajouts seront potentiellement effectués sur ce type. Il faudra donc modifier la fonction init_params en conséquence.
*)
type t_params = {margin : int ; cell_size : int ; message_size : int ; grid_size : int; ship_sizes : (string * int) list} ;;

type t_grid = (bool array) array ;;

type t_direc = DROITE | GAUCHE | HAUT | BAS ;;

type t_ship = {name : string ; size : int ; position : (int * int) ; direction : t_direc} ;;

(**
Prend en paramètre 4 entiers correspondants aux paramètres du jeu voulus (taille de la marge, des cellules,
de la zone d'affichage des messages de consignes et des grilles) et renvoie un type t_params avec les valeurs entrées.
Renvoie une erreur quand au moins une des valeurs est negative ou nulle.
Exemple d'exécution :
# init_params (30, 15, 60, 10) ;;
- : t_params = {margin = 30; cell_size = 15; message_size = 60; grid_size = 10}
*)
let init_params (p_margin, p_cell_size, p_message_size, p_grid_size, p_ship_sizes : int * int * int * int * (string * int) list) : t_params =
  if p_margin <= 0 || p_cell_size <= 0 || p_message_size <= 0 || p_grid_size <= 0 then
    failwith "Les valeurs des paramètres sont non conformes"
  else
    {margin = p_margin ; cell_size = p_cell_size ; message_size = p_message_size ; grid_size = p_grid_size ; ship_sizes = p_ship_sizes}
;;


(**
Prend en paramètres une position p_pos (int*int) et un t_params, et affiche une grille vide avec ses coordonnées à la position voulue,
selon les valeurs des paramètres du jeu.
Ne renvoie rien.
*)
let display_grid(p_pos, p_params : (int * int) * t_params) : unit =
  CPgraphics.draw_rect(fst(p_pos) + p_params.cell_size, snd(p_pos) + p_params.message_size , p_params.cell_size * p_params.grid_size, p_params.cell_size * p_params.grid_size);
  for i = 0 to 8 do
    CPgraphics.draw_rect((fst(p_pos) + (p_params.cell_size * (i + 2))), (snd(p_pos) + p_params.message_size), 1, (p_params.cell_size * p_params.grid_size));
    CPgraphics.draw_rect((fst(p_pos) + p_params.cell_size), (snd(p_pos) + p_params.message_size + (p_params.cell_size * (i + 1))), (p_params.cell_size * p_params.grid_size), 1)
  done ;
  for k = 0 to 1 do
    for i = 1 to p_params.grid_size do
      CPgraphics.moveto (p_params.margin + k * (p_params.margin + (p_params.grid_size + 1) * p_params.cell_size), p_params.margin + p_params.message_size + 10 * p_params.cell_size - i * p_params.cell_size) ;
      CPgraphics.draw_string (string_of_int (i))
    done ;
    for j = 1 to p_params.grid_size do
      CPgraphics.moveto (p_params.margin + p_params.cell_size + k * (p_params.margin + (p_params.grid_size + 1) * p_params.cell_size) + (j - 1) * p_params.cell_size, p_params.margin + p_params.message_size + p_params.grid_size * p_params.cell_size) ;
      CPgraphics.draw_char (char_of_int(int_of_char('A') + j - 1))
    done
  done
;;



(**
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
  CPgraphics.moveto (p_params.margin + p_params.cell_size, p_params.margin + p_params.message_size + (p_params.grid_size + 2) * p_params.cell_size) ;
  CPgraphics.draw_string ("Ordinateur") ;
  CPgraphics.moveto (2 * p_params.margin + (p_params.grid_size + 2) * p_params.cell_size, p_params.margin + p_params.message_size + (p_params.grid_size + 2) * p_params.cell_size) ;
  CPgraphics.draw_string ("Joueur")
;;


let create_bool_matrix (x, y : int * int) : (bool array) array =
  Array.make_matrix x y false
;;


let rec place_ship (p_positions_ship, p_grille : (int * int) list * (bool array) array) : unit =
  if p_positions_ship <> [] then
    (
      let pos : int * int = List.hd(p_positions_ship)
      in
      (
      p_grille.(fst(pos)).(snd(pos)) <- true ;
      place_ship (List.tl (p_positions_ship), p_grille)
      )
    )
;;



let positions_list (p_ship : t_ship) : (int * int) list =
  let pos_liste : (int * int) list ref = ref [p_ship.position]
  in
  if p_ship.direction = DROITE then
    for i = 1 to p_ship.size - 1 do
      pos_liste := (fst(p_ship.position) + i, snd(p_ship.position)) :: !pos_liste
    done
  else
    if p_ship.direction = GAUCHE then
      for i = 1 to p_ship.size - 1 do
        pos_liste := (fst(p_ship.position) - i, snd(p_ship.position)) :: !pos_liste
      done
    else
      if p_ship.direction = HAUT then
        for i = 1 to p_ship.size - 1 do
          pos_liste := (fst(p_ship.position), snd(p_ship.position) + i) :: !pos_liste
        done
      else
        if p_ship.direction = BAS then
          for i = 1 to p_ship.size - 1 do
            pos_liste := (fst(p_ship.position), snd(p_ship.position) - i) :: !pos_liste
          done ;
  !pos_liste
;;


let randomizer_pos () : int * int=
  Random.self_init ();
  let rand_x = Random.int 10
  and rand_y = Random.int 10 in
  rand_x, rand_y
;;

let randomizer_direc () : t_direc =
  let rand_int : int = Random.int 4 in
  if rand_int = 0 then
    DROITE
  else
    if rand_int = 1 then
      GAUCHE
    else
      if rand_int = 2 then
        HAUT
      else
        BAS
;;


let rec can_place_ship(p_params, p_list, p_grid : t_params * (int * int) list * t_grid): bool =
  if p_list = [] then
    true
  else
    let pos : int * int = List.hd (p_list) in
    if (fst(pos) >= 0  && fst(pos) <= p_params.grid_size - 1) && (snd(pos) >= 0  && snd(pos) <= p_params.grid_size - 1) && (p_grid.(fst(pos)).(snd(pos)) = false) then
      can_place_ship(p_params, List.tl(p_list), p_grid)
    else
      false
;;

let rec auto_placing_ship (p_params, p_ships_list, p_grille : t_params * (string * int) list * (bool array) array) : unit =
  if p_ships_list <> [] then
    (
      let ship : t_ship = {name = fst(List.hd(p_ships_list)) ; size = snd(List.hd(p_ships_list)) ; position = randomizer_pos() ; direction = randomizer_direc()}
      in
      let pos_list : (int * int) list = positions_list (ship)
      in
      if can_place_ship (p_params, pos_list, p_grille) then
        (
          place_ship (pos_list, p_grille) ;
          auto_placing_ship (p_params, List.tl (p_ships_list), p_grille)
        )
      else
        auto_placing_ship (p_params, p_ships_list, p_grille)
    )
;;

let color_cell (p_params, p_display_pos : t_params * (int * int)) : unit =
  CPgraphics.set_color(CPgraphics.grey);
  CPgraphics.fill_rect(fst(p_display_pos), snd(p_display_pos), p_params.cell_size, p_params.cell_size -1);
  CPgraphics.set_color(CPgraphics.black);
;;


let cell_to_pixel (p_params, p_playernum, p_pos : t_params * int * (int * int)) : (int * int) =
  let origin : int * int = (p_params.margin + (p_playernum * (p_params.cell_size * (p_params.grid_size + 1) + p_params.margin))), p_params.margin + p_params.message_size + (p_params.grid_size - 1) * p_params.cell_size + 1
  in
	((fst(origin) + p_params.cell_size * (fst(p_pos) + 1)), (snd(origin) - p_params.cell_size * snd(p_pos)))
;;

(*playernum = 0 ordi, = 1 joueur*)
let display_cell_grid(p_params, p_grid, p_playernum : t_params * t_grid * int) : unit =
  for y = 0 to p_params.grid_size - 1 do
	for x = 0 to p_params.grid_size - 1 do
  	if p_grid.(x).(y) then
    	color_cell(p_params, cell_to_pixel(p_params, p_playernum, (x, y)))
	done
  done;
  display_empty_grid(p_params)
;;




(**
Fonction d'affichage.
Ne prend rien en paramètre, initialise un t_params et ouvre la fenêtre graphique aux dimensions appropriées , et met à jour son titre.
Cette fonction ne renvoie rien.
*)
let battleship_game () : unit =
  let params : t_params = init_params (30, 15, 60, 10, [("Porte-avions", 5); ("Croiseur", 4); ("Contre-torpilleur", 3); ("Contre-torpilleur", 3); ("Torpilleur", 2)])
  in
  (
    CPgraphics.open_graph (3 * params.margin + 2 * (params.grid_size + 1) * params.cell_size, 2 * params.margin + params.message_size +(params.grid_size + 2) * params.cell_size) ;
    display_empty_grid(params);
    CPgraphics.set_window_title ("Battleship GOTY edition");
    let grille_ordi : t_grid = create_bool_matrix (10, 10)
    in
    (
      auto_placing_ship(params, params.ship_sizes, grille_ordi) ;
      display_cell_grid (params, grille_ordi, 1)
    )
  )
;;

battleship_game() ;;
read_line();;