#mod_use "camlbrick.ml" ;;
open Camlbrick ;;
#mod_use "CPtest.ml" ;;
open CPtest ;;

(*
===========================================================================================================
*)

(**
  Cette fonction détecte si un point (x,y) se trouve à l'intérieur d'un rectangle formé

  let is_inside_quad(x1,y1,x2,y2, x,y : int * int * int * int * int * int) : bool = ... ;;

  @author Nolan LAURIOUX
*)

let test_fonc_is_inside_quad() : unit =
  test_reset_report();


  let test_fonc_is_inside_quad_true() : unit =
    let l_res : bool t_test_result = test_exec(is_inside_quad , "is_inside_quad(1 , 4 , 3 , 6 , 2 , 5)" , (1 , 4 , 3 , 6 , 2 , 5)) in
  assert_equals_result(true , l_res)
  in

  let test_fonc_is_inside_quad_false() : unit =
    let l_res : bool t_test_result = test_exec(is_inside_quad , "is_inside_quad(9 , 4 , 3 , 6 , 2 , 5)" , (9 , 4 , 3 , 6 , 2 , 5)) in
  assert_equals_result(false , l_res)
  in

  test_fonc_is_inside_quad_true();
  test_fonc_is_inside_quad_false();

  test_report();
;;
test_fonc_is_inside_quad();;




(**
  Cette fonction permet de détecter si un point (x,y) se trouve à l'intérieur d'un disque de centre (cx,cy) de
  rayon rad.

  let is_inside_circle(cx,cy,rad, x, y : int * int * int * int * int) : bool = ... ;;

  @author Nolan LAURIOUX
*)

let test_fonc_is_inside_circle() : unit =
  test_reset_report();
  
  let test_fonc_is_inside_circle_true() : unit =
    let l_res : bool t_test_result = test_exec(is_inside_circle, "is_inside_circle(3 , 1 , 7 , 2 , 6)", (3 , 1 , 7 , 2 , 6)) in
  assert_equals_result(true , l_res) 
  in

  let test_fonc_is_inside_circle_false() : unit =
    let l_res : bool t_test_result = test_exec(is_inside_circle, "is_inside_circle(3 , 1 , 4 , 2 , 6)", (3 , 1 , 4 , 2 , 6)) in
  assert_equals_result(false , l_res) 
  in

  test_fonc_is_inside_circle_true();
  test_fonc_is_inside_circle_false();

  test_report();
;;
test_fonc_is_inside_circle();;

(**
  Fonction qui filtre une liste de balles en retournant cette même liste sans les balles qui quittent
  la bordure
  
  let ball_remove_out_of_border(game, balls : t_camlbrick * t_ball list) : int = ... ;;

  @author Ibraguim KARSAMOV
*)

let test_fonc_ball_remove_out_border() : unit =
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
  let l_ball_in_border : t_ball = 
    {
      position = {x = ref 10 ; y = ref 10} ;
      size = BS_MEDIUM ;
      speed = {x = 2 ; y = 5}
    }
  in
  let l_ball_out_of_border : t_ball = 
    {
      position = {x = ref 10 ; y = ref -50} ;
      size = BS_MEDIUM ;
      speed = {x = 2 ; y = 5}
    }
  in
  let l_balls : t_ball list = [l_ball_in_border ; l_ball_out_of_border] 
  in
  let l_balls_1 : t_ball list = [l_ball_out_of_border ; l_ball_out_of_border] 
  in
  let l_game : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = l_balls
    }
  in
  let l_game_1 : t_camlbrick = 
    {
      param = l_param ;
      grid = l_grid ;
      gamestate = l_gamestate ;
      paddle = l_paddle ;
      balls = l_balls_1
    }
  in

  test_reset_report ();
  (*
    Test d'une liste avec une balle en jeu et une balle hors jeu 
  *)
  let test_fonc_ball_remove_out_border_0() : unit =
    let l_res : t_ball list t_test_result = 
      test_exec(ball_remove_out_of_border , "ball_remove_out_of_border(l_game, l_balls)" , (l_game, l_balls))
    in
    assert_equals_result([l_ball_in_border], l_res);
  in
  (*
    Test avec seulement des balles hors-jeu
  *)
  let test_fonc_ball_remove_out_border_1() : unit =
    let l_res : t_ball list t_test_result = 
      test_exec(ball_remove_out_of_border , "ball_remove_out_of_border(l_game_1, l_balls_1)" , (l_game_1, l_balls_1))
    in
    assert_equals_result([], l_res);
  in

  test_fonc_ball_remove_out_border_0();
  test_fonc_ball_remove_out_border_1();
  test_report();
;;

test_fonc_ball_remove_out_border();;