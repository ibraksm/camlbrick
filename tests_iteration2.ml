#mod_use "camlbrick.ml" ;;
open Camlbrick ;;
#mod_use "CPtest.ml" ;;
open CPtest ;;

(*
===========================================================================================================
*)

(**
  Cette fonction renvoie la largeur en pixel du rectangle
  
  let paddle_size_pixel(game : t_camlbrick) : int = ... ;;

  @author Thomas CALBERAC
*)
let test_fonc_paddle_size_pixel() : unit =

  (* _______VARIABLES LOCALES_______*)
  let l_param : t_camlbrick_param = 
    {
      world_width = 800;
      world_bricks_height = 600;
      world_empty_height = 200;
      brick_width = 40;
      brick_height = 20;
      paddle_init_width = 100;
      paddle_init_height = 20;
      time_speed = ref 20;
    }
  in
  let l_grid : t_brick_kind array array = [| [| BK_bonus |] ; [| BK_bonus |] |] in
  let l_gamestate : t_gamestate = PLAYING in
  let l_small_paddle : t_paddle = 
    {
    size = PS_SMALL ;
    position = { x = ref 5 ; y = ref 5}
    }
  in
  let l_medium_paddle : t_paddle = 
    {
    size = PS_MEDIUM ;
    position = { x = ref 5 ; y = ref 5}
    }
  in
  let l_big_paddle : t_paddle = 
    {
    size = PS_BIG ;
    position = { x = ref 5 ; y = ref 5}
    }
  in
  let l_ball : t_ball = 
    {
      name = ""
      position = {x = ref 10 ; y = ref 10} ;
      size = BS_MEDIUM ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_game_small : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_small_paddle ;
      balls = [l_ball] 
    }
  in
  let l_game_medium : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_medium_paddle ;
      balls = [l_ball] 
    }
  in
  let l_game_big : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_big_paddle ;
      balls = [l_ball] 
    }
  in
  test_reset_report ();
  
  let test_fonc_paddle_size_pixel_small() : unit =
    let l_res : int t_test_result = 
      test_exec(paddle_size_pixel , "paddle_size_pixel(l_game_small)" , l_game_small)
    in
    assert_equals_result(50, l_res);
  in

  let test_fonc_paddle_size_pixel_medium() : unit =
    let l_res : int t_test_result = 
      test_exec(paddle_size_pixel , "paddle_size_pixel(l_game_medium)" , l_game_medium)
    in
  assert_equals_result(75, l_res);
  in

  let test_fonc_paddle_size_pixel_big() : unit =
    let l_res : int t_test_result = 
      test_exec(paddle_size_pixel , "paddle_size_pixel(l_game_big)" , l_game_big)
    in
  assert_equals_result(100, l_res);
  in

  test_fonc_paddle_size_pixel_small();
  test_fonc_paddle_size_pixel_medium();
  test_fonc_paddle_size_pixel_big();
  test_report();
;;
test_fonc_paddle_size_pixel();;

(*
===========================================================================================================
*)
(**
  Fonction qui à partir d'une partie récupère la position de la 
  partie gauche de la raquette

  let paddle_x(game : t_camlbrick) : int = ... ;;

  @author Thomas CALBERAC
*)

let test_fonc_paddle_x() : unit =

  (* _______VARIABLES LOCALES_______*)
  let l_param : t_camlbrick_param = 
    {
      world_width = 800;
      world_bricks_height = 600;
      world_empty_height = 200;
      brick_width = 40;
      brick_height = 20;
      paddle_init_width = 100;
      paddle_init_height = 20;
      time_speed = ref 20;
    }
  in
  let l_grid : t_brick_kind array array = [| [| BK_bonus |] ; [| BK_bonus |] |] in
  let l_gamestate : t_gamestate = PLAYING in
  let l_small_paddle : t_paddle = 
    {
    size = PS_SMALL ;
    position = { x = ref 0 ; y = ref 0}
    }
  in
  let l_medium_paddle : t_paddle = 
    {
    size = PS_MEDIUM ;
    position = { x = ref 0 ; y = ref 0}
    }
  in
  let l_big_paddle : t_paddle = 
    {
    size = PS_BIG ;
    position = { x = ref 0 ; y = ref 0}
    }
  in
  let l_ball : t_ball = 
    {
      position = {x = ref 10 ; y = ref 10} ;
      size = BS_MEDIUM ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_game_small : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_small_paddle ;
      balls = [l_ball] 
    }
  in
  let l_game_medium : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_medium_paddle ;
      balls = [l_ball] 
    }
  in
  let l_game_big : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_big_paddle ;
      balls = [l_ball] 
    }
  in

  test_reset_report();

  let test_fonc_paddle_paddle_x_big() : unit =
    let l_res : int t_test_result = 
      test_exec(paddle_x , "paddle_x(l_game_big)" , l_game_big)
    in
    assert_equals_result(-50, l_res);
  in

  let test_fonc_paddle_paddle_x_medium() : unit =
    let l_res : int t_test_result = 
      test_exec(paddle_x , "paddle_x(l_game_medium)" , l_game_medium)
    in
    assert_equals_result(-37, l_res);
  in

  let test_fonc_paddle_paddle_x_small() : unit =
    let l_res : int t_test_result = 
      test_exec(paddle_x , "paddle_x(l_game_big)" , l_game_small)
    in
    assert_equals_result(-25, l_res);
  in

  test_fonc_paddle_paddle_x_big();
  test_fonc_paddle_paddle_x_medium();
  test_fonc_paddle_paddle_x_small();
  test_report();
;;
test_fonc_paddle_x();;

(*
===========================================================================================================
*)

(**
  Cette fonction indique si la partie en cours possède des balles.
  
  let has_ball(game : t_camlbrick) : bool = ... ;;

  @author Nolan LAURIOUX
*)

let test_fonc_has_ball() : unit =

  (* _______VARIABLES LOCALES_______*)
  let l_param : t_camlbrick_param = 
    {
      world_width = 800;
      world_bricks_height = 600;
      world_empty_height = 200;
      brick_width = 40;
      brick_height = 20;
      paddle_init_width = 100;
      paddle_init_height = 20;
      time_speed = ref 20
    }
  in
  let l_grid : t_brick_kind array array = [| [| BK_bonus |] ; [| BK_bonus |] |] in
  let l_gamestate : t_gamestate = PLAYING in
  let l_paddle : t_paddle = 
    {
    size = PS_SMALL ;
    position = { x = ref 5 ; y = ref 5}
    }
  in
  let l_ball : t_ball = 
    {
      position = {x = ref 10 ; y = ref 10} ;
      size = BS_SMALL ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_game_true : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_ball] 
    }
  in
  let l_game_false : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [] 
    }
  in
  test_reset_report ();
  
  let test_fonc_has_ball_true() : unit =
    let l_res : bool t_test_result = 
      test_exec(has_ball , "has_ball(l_game_true)" , l_game_true)
    in
    assert_equals_result(true, l_res);
  in

  let test_fonc_has_ball_false() : unit =
    let l_res : bool t_test_result = 
      test_exec(has_ball , "has_ball(l_game_false)" , l_game_false)
    in
    assert_equals_result(false, l_res);
  in

  test_fonc_has_ball_true() ;
  test_fonc_has_ball_false() ;
  test_report();
;;
test_fonc_has_ball();;

(*
===========================================================================================================
*)

(**
  Fonction qui compte le nombre de balles dans une partie
  
  let balls_count(game : t_camlbrick) : int = ... ;;

  @author Nolan LAURIOUX
*)
let test_fonc_balls_count() : unit =

  (* _______VARIABLES LOCALES_______*)
  let l_param : t_camlbrick_param = 
    {
      world_width = 800;
      world_bricks_height = 600;
      world_empty_height = 200;
      brick_width = 40;
      brick_height = 20;
      paddle_init_width = 100;
      paddle_init_height = 20;
      time_speed = ref 20;
    }
  in
  let l_grid : t_brick_kind array array = [| [| BK_bonus |] ; [| BK_bonus |] |] in
  let l_gamestate : t_gamestate = PLAYING in
  let l_paddle : t_paddle = 
    {
    size = PS_SMALL ;
    position = { x = ref 5 ; y = ref 5}
    }
  in
  let l_ball : t_ball = 
    {
      position = {x = ref 10 ; y = ref 10} ;
      size = BS_SMALL ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_game_one : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_ball] 
    }
  in  
  let l_game_zero : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [] 
    }
  in
  test_reset_report ();
  
  let test_fonc_balls_count_one() : unit =
    let l_res : int t_test_result = 
      test_exec(balls_count , "balls_count(l_game_one)" , l_game_one)
    in
    assert_equals_result(1, l_res);
  in
  let test_fonc_balls_count_zero() : unit =
    let l_res : int t_test_result = 
      test_exec(balls_count , "balls_count(l_game_zero)" , l_game_zero)
    in
    assert_equals_result(0, l_res);
  in

  test_fonc_balls_count_one() ;
  test_fonc_balls_count_zero() ;
  test_report();
;;
test_fonc_balls_count();;

(*
===========================================================================================================
*)

(**
  Fonction qui donne la liste des balles d'une partie
  let balls_get(game : t_camlbrick) : t_ball list =  ... ;;

  @author Nolan LAURIOUX
*)
let test_fonc_balls_get() : unit =

  (* _______VARIABLES LOCALES_______*)
  let l_param : t_camlbrick_param = 
    {
      world_width = 800;
      world_bricks_height = 600;
      world_empty_height = 200;
      brick_width = 40;
      brick_height = 20;
      paddle_init_width = 100;
      paddle_init_height = 20;
      time_speed = ref 20;
    }
  in
  let l_grid : t_brick_kind array array = [| [| BK_bonus |] ; [| BK_bonus |] |] in
  let l_gamestate : t_gamestate = PLAYING in
  let l_paddle : t_paddle = 
    {
    size = PS_SMALL ;
    position = { x = ref 5 ; y = ref 5}
    }
  in
  let l_ball : t_ball = 
    {
      position = {x = ref 10 ; y = ref 10} ;
      size = BS_SMALL ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_game_list : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_ball] 
    }
  in  
  let l_game_list_empty : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [] 
    }
  in
  test_reset_report ();
  
  let test_fonc_balls_get_list() : unit =
    let l_res : (t_ball list) t_test_result = 
      test_exec(balls_get , "balls_get(l_game_list)" , l_game_list)
    in
    assert_equals_result([{position = {x = ref 10 ; y = ref 10} ; size = BS_SMALL ; speed = {x = 2 ; y= 5}}] , l_res);
  in
  let test_fonc_balls_get_list_empty() : unit =
    let l_res : (t_ball list) t_test_result = 
      test_exec(balls_get , "balls_get(l_game_list_empty)" , l_game_list_empty)
    in
    assert_equals_result([], l_res);
  in

  test_fonc_balls_get_list() ;
  test_fonc_balls_get_list_empty() ;
  test_report();
;;
test_fonc_balls_get();;

(*
===========================================================================================================
*)

(**
  fonction qui x renvoie l'abscisse du centre d'une balle.

  let ball_x(game,ball : t_camlbrick * t_ball) : int =  ... ;;

  @author Nolan LAURIOUX
*)
let test_fonc_ball_x() : unit =

  (* _______VARIABLES LOCALES_______*)
  let l_param : t_camlbrick_param = 
    {
      world_width = 800;
      world_bricks_height = 600;
      world_empty_height = 200;
      brick_width = 40;
      brick_height = 20;
      paddle_init_width = 100;
      paddle_init_height = 20;
      time_speed = ref 20;
    }
  in
  let l_grid : t_brick_kind array array = [| [| BK_bonus |] ; [| BK_bonus |] |] in
  let l_gamestate : t_gamestate = PLAYING in
  let l_paddle : t_paddle = 
    {
    size = PS_SMALL ;
    position = { x = ref 5 ; y = ref 5}
    }
  in
  let l_ball_pos : t_ball = 
    {
      position = {x = ref 2 ; y = ref 5} ;
      size = BS_SMALL ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_ball_zero : t_ball = 
    {
      position = {x = ref 0 ; y = ref 0} ;
      size = BS_SMALL ;
      speed = {x = 0 ; y = 5}
    }
  in
  let l_ball_neg : t_ball = 
    {
      position = {x = ref (-2) ; y = ref 10} ;
      size = BS_SMALL ;
      speed = {x = -2 ; y= 5}
    }
  
  in
  let l_game_pos : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_ball_pos] 
    }
  in
  let l_game_zero : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_ball_zero] 
    }
  in
  let l_game_neg : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_ball_neg] 
    }
  in
  test_reset_report ();
  
  let test_fonc_ball_x_pos() : unit =
    let l_res : int t_test_result = 
      test_exec(ball_x , "ball_x(l_game_pos , l_ball_pos)" , (l_game_pos , l_ball_pos))
    in
    assert_equals_result(2, l_res);
  in
  let test_fonc_ball_x_zero() : unit =
    let l_res : int t_test_result = 
      test_exec(ball_x , "ball_x(l_game_zero , l_ball_zero)" , (l_game_zero , l_ball_zero))
    in
    assert_equals_result(0, l_res);
  in
  let test_fonc_ball_x_neg() : unit =
    let l_res : int t_test_result = 
      test_exec(ball_x , "ball_x(l_game_neg, l_ball_neg)" , (l_game_neg, l_ball_neg))
    in
    assert_equals_result(-2, l_res);
  in
  test_fonc_ball_x_pos() ;
  test_fonc_ball_x_zero() ;
  test_fonc_ball_x_neg() ;
  test_report();
;;
test_fonc_ball_x();;

(*
===========================================================================================================
*)

(**
    fonction qui y renvoie l'ordonnée du centre d'une balle.

  
 let ball_y(game,ball : t_camlbrick * t_ball) : int =  ... ;;

  @author Nolan LAURIOUX
*)
let test_fonc_ball_y() : unit =

  (* _______VARIABLES LOCALES_______*)
  let l_param : t_camlbrick_param = 
    {
      world_width = 800;
      world_bricks_height = 600;
      world_empty_height = 200;
      brick_width = 40;
      brick_height = 20;
      paddle_init_width = 100;
      paddle_init_height = 20;
      time_speed = ref 20;
    }
  in
  let l_grid : t_brick_kind array array = [| [| BK_bonus |] ; [| BK_bonus |] |] in
  let l_gamestate : t_gamestate = PLAYING in
  let l_paddle : t_paddle = 
    {
    size = PS_SMALL ;
    position = { x = ref 5 ; y = ref 5}
    }
  in
  let l_ball_pos : t_ball = 
    {
      position = {x = ref 2 ; y = ref 5} ;
      size = BS_SMALL ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_ball_zero : t_ball = 
    {
      position = {x = ref 0 ; y = ref 0} ;
      size = BS_SMALL ;
      speed = {x = 0 ; y = 5}
    }
  in
  let l_ball_neg : t_ball = 
    {
      position = {x = ref 2 ; y = ref (-5)} ;
      size = BS_SMALL ;
      speed = {x = -2 ; y= 5}
    }
  
  in
  let l_game_pos : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_ball_pos] 
    }
  in
  let l_game_zero : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_ball_zero] 
    }
  in
  let l_game_neg : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_ball_neg] 
    }
  in
  test_reset_report ();
  
  let test_fonc_ball_y_pos() : unit =
    let l_res : int t_test_result = 
      test_exec(ball_y , "ball_y(l_game_pos , l_ball_pos)" , (l_game_pos , l_ball_pos))
    in
    assert_equals_result(5, l_res);
  in
  let test_fonc_ball_y_zero() : unit =
    let l_res : int t_test_result = 
      test_exec(ball_y , "ball_y(l_game_zero , l_ball_zero)" , (l_game_zero , l_ball_zero))
    in
    assert_equals_result(0, l_res);
  in
  let test_fonc_ball_y_neg() : unit =
    let l_res : int t_test_result = 
      test_exec(ball_y , "ball_y(l_game_neg, l_ball_neg)" , (l_game_neg, l_ball_neg))
    in
    assert_equals_result(-5, l_res);
  in
  test_fonc_ball_y_pos() ;
  test_fonc_ball_y_zero() ;
  test_fonc_ball_y_neg() ;
  test_report();
;;
test_fonc_ball_y();;

(*
===========================================================================================================
*)

(**
  Fonction qui renvoie la taille d'une balle
  
  let ball_size_pixel(game, ball : t_camlbrick * t_ball) : int = ... ;;

  @author Thomas CALBERAC
*)
let test_fonc_ball_size_pixel() : unit =

  (* _______VARIABLES LOCALES_______*)
  let l_param : t_camlbrick_param = 
    {
      world_width = 800;
      world_bricks_height = 600;
      world_empty_height = 200;
      brick_width = 40;
      brick_height = 20;
      paddle_init_width = 100;
      paddle_init_height = 20;
      time_speed = ref 20;
    }
  in
  let l_grid : t_brick_kind array array = [| [| BK_bonus |] ; [| BK_bonus |] |] in
  let l_gamestate : t_gamestate = PLAYING in
  let l_paddle : t_paddle = 
    {
    size = PS_MEDIUM ;
    position = { x = ref 5 ; y = ref 5}
    }
  in
  let l_small_ball : t_ball = 
    {
      position = {x = ref 10 ; y = ref 10} ;
      size = BS_SMALL ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_medium_ball : t_ball = 
    {
      position = {x = ref 10 ; y = ref 10} ;
      size = BS_MEDIUM ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_big_ball : t_ball = 
    {
      position = {x = ref 10 ; y = ref 10} ;
      size = BS_BIG ;
      speed = {x = 2 ; y= 5}
    }
  in
  let l_game : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = [l_big_ball ; l_small_ball ; l_medium_ball] 
    }
  in

  test_reset_report ();

  let test_fonc_ball_size_pixel_small() : unit =
    let l_res : int t_test_result = 
      test_exec(ball_size_pixel , "ball_size_pixel(l_game,l_small_ball)" , (l_game , l_small_ball))
    in
    assert_equals_result(5, l_res);
  in
    
  let test_fonc_ball_size_pixel_medium() : unit =
    let l_res : int t_test_result = 
      test_exec(ball_size_pixel , "ball_size_pixel(l_game,l_medium_ball)" , (l_game , l_medium_ball))
    in
  assert_equals_result(10, l_res);
  in

  let test_fonc_ball_size_pixel_big() : unit =
    let l_res : int t_test_result = 
      test_exec(ball_size_pixel , "ball_size_pixel(l_game,l_big_ball)" , (l_game , l_big_ball))
    in
  assert_equals_result(20, l_res);
  in

  test_fonc_ball_size_pixel_small();
  test_fonc_ball_size_pixel_medium();
  test_fonc_ball_size_pixel_big();
  test_report();
;;
test_fonc_ball_size_pixel();;