(**
Ce module Camlbrick représente le noyau fonctionnel du jeu de casse-brique nommé <b>camlbrick</b>
(un jeu de mot entre le jeu casse-brique et le mot ocaml).

Le noyau fonctionnel consiste à réaliser l'ensemble des structures et autres fonctions capables
d'être utilisées par une interface graphique. Par conséquent, dans ce module il n'y a aucun
aspect visuel! Vous pouvez utiliser le mode console.

Le principe du jeu de casse-brique consiste à faire disparaître toutes les briques d'un niveau
en utilisant les rebonds d'une balle depuis une raquette contrôlée par l'utilisateur.

@author Nolan LAURIOUX
@author Thomas CALBERAC
@author Ibraguim KARSAMOV
@author Manal ALOUANI

@version 1
*)

(** Compteur utilisé en interne pour afficher le numéro de la frame du jeu vidéo. 
    Vous pouvez utiliser cette variable en lecture, mais nous ne devez pas modifier
    sa valeur! *)
let frames = ref 0;;

(**
  type énuméré représentant les couleurs gérables par notre moteur de jeu. Vous ne pouvez pas modifier ce type!
  @deprecated Ne pas modifier ce type! 
*)
type t_camlbrick_color = WHITE | BLACK | GRAY | LIGHTGRAY | DARKGRAY | BLUE | RED | GREEN | YELLOW | CYAN | MAGENTA | ORANGE | LIME | PURPLE;;

(**
  Cette structure regroupe tous les attributs globaux,
  pour paramétrer notre jeu vidéo.
  <b>Attention:</b> Il doit y avoir des cohérences entre les différents paramètres:
  <ul>
  <li> la hauteur totale de la fenêtre est égale à la somme des hauteurs de la zone de briques du monde et
  de la hauteur de la zone libre.</li>
  <li>la hauteur de la zone des briques du monde est un multiple de la hauteur d'une seule brique. </li>
  <li>la largeur du monde est un multiple de la largeur d'une seule brique. </li>
  <li>initialement la largeur de la raquette doit correspondre à la taille moyenne.</li>
  <li>la hauteur initiale de la raquette doit être raisonnable et ne pas toucher un bord de la fenêtre.</li>
  <li>La variable <u>time_speed</u> doit être strictement positive. Et représente l'écoulement du temps.</li>
  </ul>
*)
type t_camlbrick_param = {
  world_width : int; (** largeur de la zone de dessin des briques *)
  world_bricks_height : int; (** hauteur de la zone de dessin des briques *)
  world_empty_height : int; (** hauteur de la zone vide pour que la bille puisse évoluer un petit peu *)

  brick_width : int; (** largeur d'une brique *)
  brick_height : int; (** hauteur d'une brique *)

  paddle_init_width : int ; (** largeur initiale de la raquette *)
  paddle_init_height : int; (** hauteur initiale de la raquette *)

  time_speed : int ref; (** indique l'écoulement du temps en millisecondes (c'est une durée approximative) *)
};;

(** Enumeration des différents types de briques. 
  Vous ne devez pas modifier ce type.    
*)
type t_brick_kind = BK_empty | BK_simple | BK_double | BK_block | BK_bonus;;

(**
  Cette fonction renvoie le type de brique pour représenter les briques de vide.
  C'est à dire, l'information qui encode l'absence de brique à un emplacement sur la grille du monde.
  @return Renvoie le type correspondant à la notion de vide.
  @deprecated  Cette fonction est utilisé en interne.    
*)
let make_empty_brick() : t_brick_kind = 
  BK_empty
;;

(** 
    Enumeration des différentes tailles des billes. 
    La taille  normale d'une bille est [BS_MEDIUM]. 
  
    Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)
type t_ball_size = BS_SMALL | BS_MEDIUM | BS_BIG;;

(** 
Enumeration des différentes taille de la raquette. Par défaut, une raquette doit avoir la taille
[PS_SMALL]. 

  Vous pouvez ajouter d'autres valeurs sans modifier les valeurs existantes.
*)
type t_paddle_size = PS_SMALL | PS_MEDIUM | PS_BIG;;



(** 
  Enumération des différents états du jeu. Nous avons les trois états de base:
    <ul>
    <li>[GAMEOVER]: qui indique si une partie est finie typiquement lors du lancement du jeu</li>
    <li>[PLAYING]: qui indique qu'une partie est en cours d'exécution</li>
    <li>[PAUSING]: indique qu'une partie en cours d'exécution est actuellement en pause</li>
    </ul>
    
    Dans le cadre des extensions, vous pouvez modifier ce type pour adopter d'autres états du jeu selon
    votre besoin.
*)
type t_gamestate = GAMEOVER | PLAYING | PAUSING;;





(**
    Type structuré d'un vecteur 2D.
    Les composantes x et y sont des entiers.
    @author Thomas CALBERAC
*)
type t_vec2 = {x : int ; y : int} ;; (* Itération 1 *)

(**
    Type structuré pour la position d'un élément
    avec x pour la coordonnée horizontale et y pour la coordonnée verticale

    @author Thomas CALBERAC
*)
type t_position = { x : int ref ; y : int ref} ;; 

(**
  Cette fonction permet de créer un vecteur 2D à partir de deux entiers.
  Les entiers représentent la composante en X et en Y du vecteur exprimé 
  avec un type structuré.

  @param p_x première composante du vecteur
  @param p_y seconde composante du vecteur
  @return Renvoie le vecteur de type t_vec2 dont les composantes sont (x,y).

  @author Thomas CALBERAC
*)
let make_vec2(p_x , p_y : int * int) : t_vec2 = 
  (* Itération 1 *)
  let l_vec : t_vec2 = {x = p_x ; y = p_y} in
  l_vec;
;;

(**
  Cette fonction renvoie un vecteur qui est la somme des deux vecteurs donnés en arguments.
  @param p_vec1 premier vecteur
  @param p_vec2 second vecteur
  @return Renvoie un vecteur égale à la somme des vecteurs.

  @author Thomas CALBERAC
*)
let vec2_add(p_vec1 , p_vec2 : t_vec2 * t_vec2) : t_vec2 =
  (* Itération 1 *)
  let l_sum_vec : t_vec2 = {x = (p_vec1.x + p_vec2.x) ; y = (p_vec1.y + p_vec2.y)} in
  l_sum_vec;
;;

(**
  Cette fonction renvoie un vecteur égale à la somme d'un vecteur
  donné en argument et un autre vecteur construit à partir de (x,y).
  
  Cette fonction est une optimisation du code suivant (que vous ne devez pas faire en l'état):
  {[
let vec2_add_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
  vec2_add(a, make_vec2(x,y))
;;
  ]}

  @param p_vec1 premier vecteur
  @param p_x composante en x du second vecteur
  @param p_y composante en y du second vecteur
  @return Renvoie un vecteur qui est la résultante du vecteur 
*)
let vec2_add_scalar(p_vec1 , p_x , p_y : t_vec2 * int * int) : t_vec2 =
  (* Itération 1 *)
  let l_sum_vec : t_vec2 = {x = (p_vec1.x + p_x) ; y = (p_vec1.y + p_y)} in
  l_sum_vec;
;;

(**
  Cette fonction calcul un vecteur où 
  ses composantes sont la résultante de la multiplication  des composantes de deux vecteurs en entrée.
  Ainsi,
    {[
    c_x = a_x * b_x
    c_y = a_y * b_y
    ]}
  @param p_vec1 premier vecteur
  @param p_vec2 second vecteur
  @return Renvoie un vecteur qui résulte de la multiplication des composantes. 
  @author Thomas CALBERAC
*)
let vec2_mult(p_vec1 , p_vec2 : t_vec2 * t_vec2) : t_vec2 = 
  (* Itération 1 *)
  let l_mult_vec : t_vec2 = {x = (p_vec1.x * p_vec2.x) ; y = (p_vec1.y * p_vec2.y)} in
  l_mult_vec;
;;

(**
  Cette fonction calcul la multiplication des composantes du vecteur a et du vecteur construit à partir de (x,y).
  Cette fonction est une optimisation du code suivant (que vous ne devez pas faire en l'état):
  {[
let vec2_mult_scalar(a,x,y : t_vec2 * int * int) : t_vec2 =
  vec2_mult(a, make_vec2(x,y))
;;
  ]}
  @param p_vec1 premier vecteur
  @param p_x composante x du second vecteur
  @param p_y composante y du second vecteur
  @return Renvoie un vecteur qui résulte de la multiplication des composantes.

  @author Thomas CALBERAC
*)
let vec2_mult_scalar(p_vec1 , p_x , p_y : t_vec2 * int * int) : t_vec2 =

(* Itération 1 *)

  let l_mult_vec : t_vec2 = {x = (p_vec1.x * p_x) ; y = (p_vec1.y * p_y)} in
  l_mult_vec;
;;

(**
  Type d'une balle 
*)
type t_ball =

(* Itération 2 *)

  {
    position : t_position ;(** position de la balle *)
    size : t_ball_size ;(** taille de la balle *)
    speed : t_vec2(** vitesse de la balle *)
  }
;;



(** 
  type de la raquette de jeu

  @author Thomas CALBERAC
*)
type t_paddle = 

(* Itération 2 *)
  {
    size : t_paddle_size ;(** taille de la raquette *)
    position : t_position (** position de la raquette *)
  }
;;

(**
    type structuré d'une brique définie par :
    <ul>
      <li>son type : [t_brick_kind]</li>
      <li>sa couleur : [t_camlbrick_color]</li>
    </ul>

    @author Thomas CALBERAC
*)
type t_camlbrick = 

(* Itération 1, 2, 3 et 4 *)

  { 
  param : t_camlbrick_param ;(** paramètres de la partie *)
  grid : t_brick_kind array array ;(** matrice contenant toutes les briques *)
  gamestate : t_gamestate ;(** état de la partie *)
  paddle : t_paddle ;(** raquette *)
  balls : t_ball list;(** balle *)
  }
;;

(**
  Cette fonction construit le paramétrage du jeu, avec des informations personnalisable avec les contraintes du sujet.
  Il n'y a aucune vérification et vous devez vous assurer que les valeurs données en argument soient cohérentes.
  @return Renvoie un paramétrage de jeu par défaut      
*)
let make_camlbrick_param() : t_camlbrick_param = {
   world_width = 800;
   world_bricks_height = 600;
   world_empty_height = 200;

   brick_width = 40;
   brick_height = 20;

   paddle_init_width = 100;
   paddle_init_height = 20;

   time_speed = ref 20;
}
;;


(**
  Cette fonction extrait le paramétrage d'un jeu à partir du jeu donné en argument.
  @param game jeu en cours d'exécution.
  @return Renvoie le paramétrage actuel.

  @author Thomas CALBERAC
  *)
let param_get(game : t_camlbrick) : t_camlbrick_param =

  (* Itération 1 *)

  game.param
;;

(**
  Cette fonction crée une raquette par défaut au milieu de l'écran et de taille normal.  
  @deprecated Cette fonction est là juste pour le debug ou pour débuter certains traitements de test.

  @author Thomas CALBERAC
*)
let make_paddle() : t_paddle =

  (* Itération 2 *)

  {
    size = PS_SMALL ;
    position = {x = ref 0  ; y = ref 0}
  }
;;

let make_ball() : t_ball =

  (* Itération 3 *)
  
  {
    position = {x = ref 0 ; y = ref 0};
    size = BS_SMALL ;
    speed = {x = 0 ; y = 0}
  }
;;
(**
  Cette fonction crée une nouvelle structure qui initialise le monde avec aucune brique visible.
  Une raquette par défaut et une balle par défaut dans la zone libre.
  On remplace ensuite aléatoirement les briques visibles avec des briques de tous types.
  
  @return Renvoie un jeu correctement initialisé
*)
let make_camlbrick() : t_camlbrick =

  (* Itération 1, 2, 3 et 4 *)

  let brick_kind : t_brick_kind array = [| BK_empty ; BK_simple ; BK_double ; BK_block ; BK_bonus |] in
  let l_param : t_camlbrick_param = make_camlbrick_param() in
  let l_grid : t_brick_kind array array = Array.make_matrix (l_param.world_width / l_param.brick_width) (l_param.world_bricks_height / l_param.brick_height) BK_empty in
  let l_paddle : t_paddle = make_paddle() in
  let l_ball : t_ball = make_ball() in

  for i = 0 to (l_param.world_width / l_param.brick_width) - 1
  do
    for j = 0 to (l_param.world_bricks_height / l_param.brick_height) - 1
    do
      l_grid.(i).(j) <- brick_kind.(Random.int(5))
    done;
  done;
  {
    param = l_param ; 
    grid = l_grid ;
    gamestate = PAUSING ;
    paddle = l_paddle ;
    balls = [l_ball]
    };
;;

(**
  Fonction utilitaire qui permet de traduire l'état du jeu sous la forme d'une chaîne de caractère.
  Cette fonction est appelée à chaque frame, et est affichée directement dans l'interface graphique.
  
  Vous devez modifier cette fonction.

  @param game représente le jeu en cours d'exécution.
  @return Renvoie la chaîne de caractère représentant l'état du jeu.

  @author Thomas CALBERAC
*)
let string_of_gamestate(game : t_camlbrick) : string =

  (* Itération 1,2,3 et 4 *)
  if game.gamestate = PLAYING
  then
    "Playing"
  else
    if game.gamestate = PAUSING
    then
      "Pausing"
    else 
      "Gameover"
;;

(**
    fonction qui récupère une brique à des coordonnées données à 
    partir d'une matrice de brique d'un partie

    @param game représente le jeu en cours d'exécution.
    @param i coordonnée y de la brique
    @param j coordonnée x de la brique

    @author Thomas CALBERAC
*)
let brick_get(game, i, j : t_camlbrick * int * int)  : t_brick_kind =

  (* Itération 1 *)

  game.grid.(i).(j)
;;

(**
    fonction qui prend en paramètre une game et les coordonnées d'une brique et renvoi
    la couleur de la brique.
     <ul>
      <li>Brique Block = Gris</li>
      <li>Brique Vide = Noir (couleur du fond d'écran)</li>
      <li>Brique Simple = Jaune</li>
      <li>Brique Bonus = Rouge</li>
      <li>Brique Double = Vert</li>
    </ul>

    @param game partie de type t_camlbrick
    @param i coordonnée en y de la brique
    @param j coordonnée en x de la brique
    @return Renvoie la couleur de la brique

    @author Thomas CALBERAC
*)
let brick_color(game , i , j : t_camlbrick * int * int) : t_camlbrick_color = 

  (* Itération 1 *)

  if brick_get(game , i , j) = BK_block
  then 
    GRAY
  else
    if brick_get(game , i , j) = BK_empty
      then 
        BLACK
      else
        if brick_get(game , i , j) = BK_simple
          then 
            YELLOW
          else
            if brick_get(game , i , j) = BK_bonus
              then 
                RED
              else
                GREEN
;;

(**
    focntion qui renvoie la taille de la raquette d'une partie donnée

    @param game partie en cours
    @return Renvoie la taille en entier de la largeur de la raquette

    @author Thomas CALBERAC
*)
let paddle_size_pixel(game : t_camlbrick) : int = 

  (* Itération 2 *)

  if game.paddle.size = PS_SMALL
  then
    50
  else
    if game.paddle.size = PS_MEDIUM
    then
      75
    else 
      100 
;;

(**
    Fonction qui à partir d'une partie récupère la position de la 
    partie gauche de la raquette

    @param game partie en cours 
    @return Renvoie la position gauche du rectangle symbolisant la raquette

    @author Thomas CALBERAC
*)
let paddle_x(game : t_camlbrick) : int= 

  (* Itération 2 *)

  let left_paddle_x : int = !(game.paddle.position.x) - (paddle_size_pixel(game) / 2) in
  left_paddle_x ; 
;;

(** 
    fonction qui déplace la raquette latéralement vers la gauche

    @param game partie en cours
    
    @author Thomas CALBERAC
*)
let paddle_move_left(game : t_camlbrick) : unit = 

  (* Itération 2 *)

  if !(game.paddle.position.x) - (paddle_size_pixel(game) /2) <= (-(game.param.world_width) / 2)
  then
    ()
  else
    game.paddle.position.x := !(game.paddle.position.x) - 5
  
;;

(**
    fonction qui déplace la raquette latéralement vers la droite

    @param game partie en cours
    
    @author Thomas CALBERAC
*)
let paddle_move_right(game : t_camlbrick) : unit =

  (* Itération 2 *)
  
  if !(game.paddle.position.x) + (paddle_size_pixel(game) /2) >= ((game.param.world_width) / 2)
  then
    ()
  else
    game.paddle.position.x := !(game.paddle.position.x) + 5
    
;;


(**
  Fonction qui test si une partie possède au moins une balle

  @param game partie en cours
  @return false si pas de balle , true si au moins une balle

  @author Thomas CALBERAC
*)
let has_ball(game : t_camlbrick) : bool =

  (* Itération 2 *)

  if game.balls = [] 
  then 
    false
  else
    true

;;

(** 
  Fonction qui compte le nombre de balles dans une partie

  @param game partie en cours
  @return Renvoie un entier représentant le nombre de balles

  @author Thomas CALBERAC
*)
let balls_count(game : t_camlbrick) : int =

  (* Itération 2 *)

  List.length(game.balls)
;;

(** 
  Fonction qui donne la liste des balles d'une partie

  @param game partie en cours
  @return Renvoie la liste des balles de la partie

  @author Thomas CALBERAC
*)
let balls_get(game : t_camlbrick) : t_ball list = 
  (* Itération 2 *)
  game.balls
;;

(**
  Fonction qui récupère la i ème balle de la liste de balle d'une partie

  @param game partie en cours
  @return Renvoie la balle du rang i

  @author Thomas CALBERAC
*)
let ball_get(game, i : t_camlbrick * int) : t_ball =
  (* Itération 2 *)
  
  List.nth (game.balls) (i)
;;

(**
  Fonction qui renvoie l'abcisse du centre d'une balle

  @param game partie en cours
  @param ball balle pour laquelle on veut récupérer l'abcisse
  @return Renvoie l'abcisse de la balle

  @author Thomas CALBERAC
*)
let ball_x(game , ball : t_camlbrick * t_ball) : int =

  (* Itération 2 *)

  let l_res : int ref = ref 0 in

  for i = 0 to List.length(game.balls) - 1
  do
    if ball_get(game , i) = ball
    then
      l_res := !((List.nth (game.balls) (i)).position.x) 
    else
      ()
  done;
  !l_res;
;;

(**
  Fonction qui renvoie l'ordonnée du centre d'une balle
  
  @param game partie en cours
  @param ball balle pour laquelle on veut récupérer l'ordonnée
  @return Renvoie l'ordonnée de la balle

  @author Thomas CALBERAC
*)
let ball_y(game, ball : t_camlbrick * t_ball) : int =

  (* Itération 2 *)

  let l_res : int ref = ref 0 in

  for i = 0 to List.length(game.balls) - 1
  do
    if ball_get(game , i) = ball
    then
      l_res := !((List.nth (game.balls) (i)).position.y)
    else
      ()
  done;
  !l_res;
;;

(**
  Fonction qui renvoie la taille d'une balle
  
  @param game partie en cours
  @param ball balle pour laquelle on veut récupérer la taille
  @return Renvoie la taille de la balle

  @author Thomas CALBERAC
*)
let ball_size_pixel(game, ball : t_camlbrick * t_ball) : int =

  (* Itération 2 *)

  let l_res : int ref = ref 0 in

  for i = 0 to (List.length(game.balls) - 1)
  do

  let l_current_ball : t_ball ref = ref (ball_get(game , i)) in

    if !l_current_ball = ball
    then (
      if !l_current_ball.size = BS_SMALL
      then
        l_res := 5 
      else
        ();
      if !l_current_ball.size = BS_MEDIUM
      then 
        l_res := 10 
      else
        ();
      if !l_current_ball.size = BS_BIG
      then 
        l_res := 20 
      else
        ();
    )
    else
      ();
  done;
  !l_res
;;

(**
  Fonction qui renvoie la couleur d'une balle
  
  @param game partie en cours
  @param ball balle pour laquelle on veut récupérer la couleur
  @return Renvoie la couleur de la balle

  @author Thomas CALBERAC
*)
let ball_color(game, ball : t_camlbrick * t_ball) : t_camlbrick_color =

  (* Itération 2 *)
  let l_res : t_camlbrick_color ref = ref WHITE in

  for i = 0 to List.length(game.balls) - 1
  do
    if List.nth (game.balls) (i) = ball
    then
      if ball.size = BS_SMALL
        then
          l_res := RED
        else
          if ball.size = BS_MEDIUM
          then
            l_res := YELLOW
          else 
            l_res := GRAY
    else
      failwith "Error ball_size_pixel : il n'y a pas la balle demandée"
  done;
  !l_res;

;;
(**
  fonction qui modifie la vitesse d'une balle par addition

  @param game partie en cours
  @param balle pour laquelle on modifie la vitesse
  @param dv vecteur que l'on va additionner à la balle
  @return Ne renvoie rien fonction qui modifie le vecteur vitesse de la balle uniquement

  @author Thomas CALBERAC
*)
let ball_modif_speed(game, ball, dv : t_camlbrick * t_ball * t_vec2) : unit =

  (* Itération 3 *)

  for i = 0 to List.length(game.balls) - 1
  do
    if List.nth (game.balls) (i) = ball
    then
      (List.nth (game.balls) (i)).speed := vec2_add(!((List.nth (game.balls) (i)).speed) , !dv)
    else
      ()
  done;
;;

(**
  fonction qui modifie la vitesse d'une balle par multiplication

  @param game partie en cours
  @param balle pour laquelle on modifie la vitesse
  @param dv vecteur que l'on va multiplier à la balle
  @return Ne renvoie rien fonction qui modifie le vecteur vitesse de la balle uniquement

  @author Thomas CALBERAC
*)
let ball_modif_speed_sign(game, ball, sv : t_camlbrick * t_ball * t_vec2) : unit =

  (* Itération 3 *)

  for i = 0 to List.length(game.balls) - 1
  do
    if List.nth (game.balls) (i) = ball
    then
      (List.nth (game.balls) (i)).speed := vec2_mult(!((List.nth (game.balls) (i)).speed) , !dv)
    else
      ()
  done;
;;

(**
  fonction qui vérifie si un point est dans un cercle

  @param cx coordonnée horizontale du centre du cercle
  @param cy coordonnée verticale du centre du cercle
  @param rad rayon du cercle 
  @param x coordonnée horizontale du point
  @param y coordonnée verticale du centre du cercle
  @return Renvoie vrai si le point est dans le cercle et faux dans le cas contraire

  @author Thomas CALBERAC
*)
let is_inside_circle(cx,cy,rad, x, y : int * int * int * int * int) : bool =

  (* Itération 3 *)
  
  (x - cx) * (x - cx) + (y - cy) * (y - cy) <= rad * rad
;;


(**
  fonction qui vérifie si un point est dans un cercle

  @param x1 coordonnée horizontale du coin gauche du rectangle
  @param y1 coordonnée horizontale du coin gauche du rectangle
  @return Renvoie vrai si le point est dans le cercle et faux dans le cas contraire

  @author Thomas CALBERAC
*)
let is_inside_quad(x1,y1,x2,y2, x,y : int * int * int * int * int * int) : bool =

  (* Itération 3 *)

  (x1 <= x) && (x <= x2) && (y1 <= y) && (y <= y2)
;;

(* cette fonction va prendre 5 ans avant d'être finie par NOLAN LAURIOUX *)
let ball_remove_out_of_border(game,balls : t_camlbrick * t_ball list ) : t_ball list = 
  (* Itération 3 *)
  balls
;;

let ball_hit_paddle(game,ball,paddle : t_camlbrick * t_ball * t_paddle) : unit =

  (* Itération 3 *)

  
;;


(* lire l'énoncé choix à faire *)
let ball_hit_corner_brick(game,ball, i,j : t_camlbrick * t_ball * int * int) : bool =
  (* Itération 3 *)
  false
;;

(* lire l'énoncé choix à faire *)
let ball_hit_side_brick(game,ball, i,j : t_camlbrick * t_ball * int * int) : bool =
  (* Itération 3 *)
  false
;;

let game_test_hit_balls(game, balls : t_camlbrick * t_ball list) : unit =
  (* Itération 3 *)
  ()
;;

(**
  Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'elle se déplace. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param x l'abscisse de la position de la souris
  @param y l'ordonnée de la position de la souris     
*)
let canvas_mouse_move(game,x,y : t_camlbrick * int * int) : unit = 
  ()
;;

(**
  Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'un bouton est enfoncé. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param button numero du bouton de la souris enfoncé.
  @param x l'abscisse de la position de la souris
  @param y l'ordonnée de la position de la souris     
*)
let canvas_mouse_click_press(game,button,x,y : t_camlbrick * int * int * int) : unit =
  ()
;;


(**
  Cette fonction est appelée par l'interface graphique avec le jeu en argument et la position
  de la souris dans la fenêtre lorsqu'un bouton est relaché. 
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param button numero du bouton de la souris relaché.
  @param x l'abscisse de la position du relachement
  @param y l'ordonnée de la position du relachement   
*)
let canvas_mouse_click_release(game,button,x,y : t_camlbrick * int * int * int) : unit =
  ()
;;



(**
  Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est appuyée.
  Les arguments sont le jeu en cours, la touche enfoncé sous la forme d'une chaine et sous forme d'un code
  spécifique à labltk.
  
  Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
  les identifiées facilement dans nos traitements.

  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param keyString nom de la touche appuyée.
  @param keyCode code entier de la touche appuyée.   
*)
let canvas_keypressed(game, keyString, keyCode : t_camlbrick * string * int) : unit =
  print_string("Key pressed: ");
  print_string(keyString);
  print_string(" code=");
  print_int(keyCode);
  print_newline();

  if keyCode = 65361 
  then 
    paddle_move_left(game)
  else
    if keyCode = 65363 
      then 
        paddle_move_right(game)
      else
        ()
;;

(**
  Cette fonction est appelée par l'interface graphique lorsqu'une touche du clavier est relachée.
  Les arguments sont le jeu en cours, la touche relachée sous la forme d'une chaine et sous forme d'un code
  spécifique à labltk.
  
  Le code fourni initialement permet juste d'afficher les touches appuyées au clavier afin de pouvoir
  les identifiées facilement dans nos traitements.

  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
  @param keyString nom de la touche relachée.
  @param keyCode code entier de la touche relachée.   
*)
let canvas_keyreleased(game, keyString, keyCode : t_camlbrick * string * int) =
  print_string("Key released: ");
  print_string(keyString);
  print_string(" code=");
  print_int(keyCode);
  print_newline()
;;

(**
  Cette fonction est utilisée par l'interface graphique pour connaitre l'information
  l'information à afficher dans la zone Custom1 de la zone du menu.
*)
let custom1_text() : string =
  (* Iteration 4 *)
  "<Rien1>"
;;

(**
  Cette fonction est utilisée par l'interface graphique pour connaitre l'information
  l'information à afficher dans la zone Custom2 de la zone du menu.
*)
let custom2_text() : string =
  (* Iteration 4 *)
  "<Rien2>"
;;


(**
  Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
  de la zone de menu et que ce bouton affiche "Start".

  
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
*)
let start_onclick(game : t_camlbrick) : unit=
  ()
;;

(**
  Cette fonction est appelée par l'interface graphique lorsqu'on clique sur le bouton
  de la zone de menu et que ce bouton affiche "Stop".

  
  Vous pouvez réaliser des traitements spécifiques, mais comprenez bien que cela aura
  un impact sur les performances si vous dosez mal les temps de calcul.
  @param game la partie en cours.
*)
let stop_onclick(game : t_camlbrick) : unit =
  ()
;;

(**
  Cette fonction est appelée par l'interface graphique pour connaitre la valeur
  du slider Speed dans la zone du menu.

  Vous pouvez donc renvoyer une valeur selon votre désir afin d'offrir la possibilité
  d'interagir avec le joueur.
*)
let speed_get(game : t_camlbrick) : int = 
  0
;;


(**
  Cette fonction est appelée par l'interface graphique pour indiquer que le 
  slide Speed dans la zone de menu a été modifiée. 
  
  Ainsi, vous pourrez réagir selon le joueur.
*)
let speed_change(game,xspeed : t_camlbrick * int) : unit=
  print_endline("Change speed : "^(string_of_int xspeed));
;;

(**
  Cette fonction crée une nouvelle structure qui initialise le monde avec aucune brique visible.
  Une raquette par défaut et une balle par défaut dans la zone libre.
  On remplace ensuite aléatoirement les briques visibles avec des briques de tous types.
  
  @return Renvoie un jeu correctement initialisé
*)
let make_camlbrick() : t_camlbrick =

  (* Itération 1, 2, 3 et 4 *)

  let brick_kind : t_brick_kind array = [| BK_empty ; BK_simple ; BK_double ; BK_block ; BK_bonus |] in
  let l_param : t_camlbrick_param = make_camlbrick_param() in
  let l_grid : t_brick_kind array array = Array.make_matrix (l_param.world_width / l_param.brick_width) (l_param.world_bricks_height / l_param.brick_height) BK_empty in
  
  for i = 0 to (l_param.world_width / l_param.brick_width) - 1
  do
    for j = 0 to (l_param.world_bricks_height / l_param.brick_height) - 1
    do
      l_grid.(i).(j) <- brick_kind.(Random.int(5))
    done;
  done;
  {param = l_param ; grid = l_grid ; gamestate = PLAYING ; paddle = make_paddle() ; ball = ()};
;;

let animate_action(game : t_camlbrick) : unit =  
  (* Iteration 1,2,3 et 4
    Cette fonction est appelée par l'interface graphique à chaque frame
    du jeu vidéo.
    Vous devez mettre tout le code qui permet de montrer l'évolution du jeu vidéo.    
  *)
  ()
;;
