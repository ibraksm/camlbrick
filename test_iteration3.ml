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

