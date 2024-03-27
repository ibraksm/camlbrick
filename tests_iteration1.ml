#mod_use "camlbrick.ml" ;;
open Camlbrick ;;

#mod_use "CPtest.ml" ;;
open CPtest ;;
(**
  Cette fonction permet de créer un vecteur 2D à partir de deux entiers.
  Les entiers représentent la composante en X et en Y du vecteur exprimé 
  avec un type structuré.

  let make_vec2(p_x , p_y : int * int) : t_vec2 = ... ;;

  @author Thomas CALBERAC
*)
let test_fonc_make_vec2() : unit =

  test_reset_report() ;

  (*On vérifie le cas où on a un vecteur avec des valeures positives *)
  let test_fonc_make_vec2_pos() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(make_vec2 , "make_vec2(2 , 2)" , (2 , 2)) in
    assert_equals_result({x = 2 ; y = 2} , l_res)
  in

  (*On vérifie le cas où on a un vecteur nul*)
  let test_fonc_make_vec2_nul() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(make_vec2 , "make_vec2(0 , 0)" , (0 , 0)) in
    assert_equals_result({x = 0 ; y = 0} , l_res)
  in

  (*On vérifie le cas où on a un vecteur avec des valeures négatives*)
  let test_fonc_make_vec2_neg() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(make_vec2 , "make_vec2(-3 , -1)" , (-3 , -1)) in
    assert_equals_result({x = -3 ; y = -1} , l_res)
  in

  test_fonc_make_vec2_pos();
  test_fonc_make_vec2_nul();
  test_fonc_make_vec2_neg();

  test_report() ;
;;
test_fonc_make_vec2();;
(*
===========================================================================================================
*)
(**
  Cette fonction renvoie un vecteur qui est la somme des deux vecteurs donnés en arguments.

  let vec2_add(p_vec1 , p_vec2 : t_vec2 * t_vec2) : t_vec2 = ... ;;

  @author Thomas CALBERAC
*)
let test_fonc_vec2_add() : unit =
  test_reset_report() ;

  (*On vérifie le cas où on additionne deux vecteurs positifs *)
  let test_fonc_vec2_add_pos() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add , "vec2_add({x = 2 ; y = 2} , {x = 4 , y = 5})" , ({x = 2 ; y = 2} , {x = 4 ; y = 5})) in
    assert_equals_result({x = 6 ; y = 7} , l_res)
  in

   (*On vérifie le cas où on additionne deux vecteurs négatifs *)
  let test_fonc_vec2_add_neg() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add , "vec2_add({x = -2 ; y = -2} , {x = -4 , y = -5})" , ({x = -2 ; y = -2} , {x = -4 ; y = -5})) in
    assert_equals_result({x = -6 ; y = -7} , l_res)
  in

  (*On vérifie le cas où on a un vecteur nul en résultat*)
  let test_fonc_vec2_add_nul() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add , "vec2_add({x = -2 ; y = -4} , {x = 2 , y = 4})" , ({x = -2 ; y = -4} , {x = 2 ; y = 4})) in
    assert_equals_result({x = 0 ; y = 0} , l_res)
  in

  test_fonc_vec2_add_neg();
  test_fonc_vec2_add_pos();
  test_fonc_vec2_add_nul();
  test_report() ;
;;
test_fonc_vec2_add();;
(*
===========================================================================================================
*)
(**
  Cette fonction renvoie un vecteur égale à la somme d'un vecteur
  donné en argument et un autre vecteur construit à partir de (x,y).

  let vec2_add_scalar(p_vec1 , p_x , p_y : t_vec2 * int * int) : t_vec2 = ... ;;

  @author Thomas CALBERAC
*)
let test_fonc_vec2_add_scalar() : unit =
  test_reset_report() ;

  (*On vérifie le cas où on additionne deux vecteurs positifs *)
  let test_fonc_vec2_add_scalar_pos() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add_scalar , "vec2_add_scalar({x = 2 ; y = 2} , 4 , 5)" , ({x = 2 ; y = 2} , 4 , 5)) in
    assert_equals_result({x = 6 ; y = 7} , l_res)
  in

   (*On vérifie le cas où on additionne deux vecteurs négatifs *)
  let test_fonc_vec2_add_scalar_neg() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add_scalar , "vec2_add_scalar({x = -2 ; y = -2} , -4 , -5)" , ({x = -2 ; y = -2} , -4 , -5)) in
    assert_equals_result({x = -6 ; y = -7} , l_res)
  in

  (*On vérifie le cas où on a un vecteur nul en résultat*)
  let test_fonc_vec2_add_scalar_nul() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add_scalar , "vec2_add_scalar({x = -2 ; y = -4} , 2 , 4)" , ({x = -2 ; y = -4} , 2 , 4)) in
    assert_equals_result({x = 0 ; y = 0} , l_res)
  in

  test_fonc_vec2_add_scalar_neg();
  test_fonc_vec2_add_scalar_pos();
  test_fonc_vec2_add_scalar_nul();
  test_report();
;;
test_fonc_vec2_add_scalar();;
(*
===========================================================================================================
*)
(**
  Cette fonction calcul un vecteur où 
  ses composantes sont la résultante de la multiplication  des composantes de deux vecteurs en entrée.

  let vec2_mult(p_vec1 , p_vec2 : t_vec2 * t_vec2) : t_vec2 = ... ;;

  @author Thomas CALBERAC
*)
let test_fonc_vec2_mutl() : unit =
  test_reset_report() ;

  (*On vérifie le cas où on multiplie deux vecteurs positifs *)
  let test_fonc_vec2_mult_pos() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_mult , "vec2_mult({x = 2 ; y = 2} , {x = 4 , y = 5})" , ({x = 2 ; y = 2} , {x = 4 ; y = 5})) in
    assert_equals_result({x = 8 ; y = 10} , l_res)
  in

   (*On vérifie le cas où on multiplie deux vecteurs négatifs *)
  let test_fonc_vec2_mult_neg() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_mult , "vec2_mult({x = -2 ; y = -2} , {x = -4 , y = -5})" , ({x = -2 ; y = -2} , {x = -4 ; y = -5})) in
    assert_equals_result({x = 8 ; y = 10} , l_res)
  in

  (*On vérifie le cas où on multiplie par un vecteur nul*)
  let test_fonc_vec2_mult_nul() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add , "vec2_add({x = -2 ; y = -4} , {x = 2 , y = 4})" , ({x = -2 ; y = -4} , {x = 2 ; y = 4})) in
    assert_equals_result({x = 0 ; y = 0} , l_res)
  in

  test_fonc_vec2_add_neg();
  test_fonc_vec2_add_pos();
  test_fonc_vec2_add_nul();
  test_report() ;
;;
test_fonc_vec2_add();;
(*
===========================================================================================================
*)
(**
  Cette fonction calcul la multiplication des composantes du vecteur a et du vecteur construit 
  à partir de (x,y).

  let vec2_mult_scalar(p_vec1 , p_x , p_y : t_vec2 * int * int) : t_vec2 = ... ;; 

  @author Thomas CALBERAC
*)
let test_fonc_vec2_mult_scalar() : unit =
  test_reset_report() ;

  (*On vérifie le cas où on multiplie deux vecteurs positifs *)
  let test_fonc_vec2_mult_scalar() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add_scalar , "vec2_add_scalar({x = 2 ; y = 2} , 4 , 5)" , ({x = 2 ; y = 2} , 4 , 5)) in
    assert_equals_result({x = 6 ; y = 7} , l_res)
  in

   (*On vérifie le cas où on additionne deux vecteurs négatifs *)
  let test_fonc_vec2_add_scalar_neg() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add_scalar , "vec2_add_scalar({x = -2 ; y = -2} , -4 , -5)" , ({x = -2 ; y = -2} , -4 , -5)) in
    assert_equals_result({x = -6 ; y = -7} , l_res)
  in

  (*On vérifie le cas où on a un vecteur nul en résultat*)
  let test_fonc_vec2_add_scalar_nul() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_add_scalar , "vec2_add_scalar({x = -2 ; y = -4} , 2 , 4)" , ({x = -2 ; y = -4} , 2 , 4)) in
    assert_equals_result({x = 0 ; y = 0} , l_res)
  in

  test_fonc_vec2_add_scalar_neg();
  test_fonc_vec2_add_scalar_pos();
  test_fonc_vec2_add_scalar_nul();
  test_report();
;;

(**
  Cette fonction vérifie qu'un failwith a été retourné quand
  des coordonnées négatives sont insérés dans la fonction brick_hit()
  @author Ibraguim KARSAMOV    
*)
let test_fonc_brick_get() : unit =
  let game : t_camlbrick = make_camlbrick()
  in
    let l_res : unit t_test_result =
      test_exec(brick_get, "brick_get(game, -1, -1)", (game, -1, -1))
    in
      assert_failwith(l_res)
;;

(**
  Cette fonction vérifie qu'un failwith a été retourné quand
  des coordonnées négatives sont insérés dans la fonction brick_hit()
  @author Ibraguim KARSAMOV    
*)
let test_fonc_brick_hit() : unit =
  let game : t_camlbrick = make_camlbrick()
  in
    let l_res : unit t_test_result =
      test_exec(brick_hit, "brick_hit(game, -1, -1)", (game, -1, -1))
    in
      assert_failwith(l_res)
;;

(**
  Cette fonction vérifie qu'un failwith a été retourné quand
  des coordonnées négatives sont insérés dans la fonction brick_color()
  @author Ibraguim KARSAMOV    
*)
let test_fonc_brick_color() : unit =
  let game : t_camlbrick = make_camlbrick()
  in
    let l_res : unit t_test_result =
      test_exec(brick_color, "brick_color(game, -1, -1)", (game, -1, -1))
    in
      assert_failwith(l_res)
;;

(* Initialisation du rapport de test. *)
test_reset_report() ;;

(* Appels des fonctions de test *)
test_fonc_vec2_add_scalar();;
test_fonc_brick_get();;
test_fonc_brick_hit();;
test_fonc_brick_color();;

(* Affiche le rapport de test *)
test_report() ;;