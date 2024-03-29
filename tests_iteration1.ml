#mod_use "camlbrick.ml" ;;
open Camlbrick ;;

#mod_use "CPtest.ml" ;;
open CPtest ;;
(*
===========================================================================================================
*)
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

  @author Nolan LAURIOUX
  @author Thomas CALBERAC
*)
let test_fonc_vec2_mult() : unit =
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
    let l_res : t_vec2 t_test_result = test_exec(vec2_mult , "vec2_mult({x = 0 ; y = 0} , {x = 2 , y = 4})" , ({x = 0 ; y = 0} , {x = 2 ; y = 4})) in
    assert_equals_result({x = 0 ; y = 0} , l_res)
  in

  test_fonc_vec2_mult_neg();
  test_fonc_vec2_mult_pos();
  test_fonc_vec2_mult_nul();
  test_report() ;
;;
test_fonc_vec2_mult();;
(*
===========================================================================================================
*)
(**
  Cette fonction calcul la multiplication des composantes du vecteur a et du vecteur construit 
  à partir de (x,y).

  let vec2_mult_scalar(p_vec1 , p_x , p_y : t_vec2 * int * int) : t_vec2 = ... ;; 

  @author Nolan LAURIOUX
*)
let test_fonc_vec2_mult_scalar() : unit =
  test_reset_report() ;

  (*On vérifie le cas où on multiplie deux vecteurs positifs *)
  let test_fonc_vec2_mult_scalar_pos() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_mult_scalar , "vec2_mult_scalar({x = 2 ; y = 3} , 4 , 5)" , ({x = 2 ; y = 3} , 4 , 5)) in
    assert_equals_result({x = 8 ; y = 15} , l_res)
  in

   (*On vérifie le cas où on multiplie deux vecteurs négatifs *)
  let test_fonc_vec2_mult_scalar_neg() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_mult_scalar , "vec2_mult_scalar({x = -2 ; y = -3} , -4 , -5)" , ({x = -2 ; y = -3} , -4 , -5)) in
    assert_equals_result({x = 8 ; y = 15} , l_res)
  in

  (*On vérifie le cas où on a un vecteur nul en résultat*)
  let test_fonc_vec2_mult_scalar_nul() : unit = 
    let l_res : t_vec2 t_test_result = test_exec(vec2_mult_scalar , "vec2_mult_scalar({x = 0 ; y = 3} , 4 , 0)" , ({x = 0 ; y = 3} , 4 , 0)) in
    assert_equals_result({x = 0 ; y = 0} , l_res)
  in

  test_fonc_vec2_mult_scalar_neg();
  test_fonc_vec2_mult_scalar_pos();
  test_fonc_vec2_mult_scalar_nul();
  test_report();
;;
test_fonc_vec2_mult_scalar();;
(*
===========================================================================================================
*)
(** 
  Cette fonction extrait le paramétrage d'un jeu à partir du jeu donné en argument.

  let param_get(game : t_camlbrick) : t_camlbrick_param = ... ;; 

  type t_camlbrick = 
  { 
  param : t_camlbrick_param ;
  grid : t_brick_kind array array ;
  }
;;

  @author Nolan LAURIOUX
  @author Thomas CALBERAC
*)
let test_fonc_param_get() : unit = 
  let l_res : t_camlbrick_param t_test_result = 
    test_exec(
      param_get , 
      "param_get(
        {
          param = {
            world_width = 800;
            world_bricks_height = 600;
            world_empty_height = 200;
            brick_width = 40;
            brick_height = 20;
            paddle_init_width = 100;
            paddle_init_height = 20;
            time_speed = ref 20;
            } ; 
          grid = [| [| BK_bonus |] ; [| BK_bonus |] |] )
        } " , 
      (
        {
          param = {
            world_width = 800;
            world_bricks_height = 600;
            world_empty_height = 200;
   
            brick_width = 40;
            brick_height = 20;
   
            paddle_init_width = 100;
            paddle_init_height = 20;
   
            time_speed = ref 20;
            } ;
          grid = [| [| BK_bonus |] ; [| BK_bonus |] |] 
        }
      )
    )
  in
  assert_equals_result(
    {
    world_width = 800;
    world_bricks_height = 600;
    world_empty_height = 200;

    brick_width = 40;
    brick_height = 20;

    paddle_init_width = 100;
    paddle_init_height = 20;

    time_speed = ref 20;
    } , 
    l_res);
;;
test_fonc_param_get();;
test_report();;
test_reset_report();;
(*
===========================================================================================================
*)
(**
  fonction qui récupère une brique à des coordonnées données à 
  partir d'une matrice de brique d'un partie
    
  let brick_get(game, i, j : t_camlbrick * int * int)  : t_brick_kind = ... ;;
    
  @author Nolan LAURIOUX
*)  
let test_fonc_brick_get() : unit = 
  let l_res : t_brick_kind t_test_result = 
    test_exec(
      brick_get , 
      "brick_get(
        {
          param = {
            world_width = 800;
            world_bricks_height = 600;
            world_empty_height = 200;
            brick_width = 40;
            brick_height = 20;
            paddle_init_width = 100;
            paddle_init_height = 20;
            time_speed = ref 20;
            } ; 
            grid = 
            [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
                [| BK_bonus ; BK_bonus ; BK_bonus |] ;
                [| BK_bonus ; BK_bonus ; BK_empty |] |] 
          }   
        } ,
        2 ,
        2 " , 
      (
        {
          param = {
            world_width = 800;
            world_bricks_height = 600;
            world_empty_height = 200;
   
            brick_width = 40;
            brick_height = 20;
   
            paddle_init_width = 100;
            paddle_init_height = 20;
   
            time_speed = ref 20;
            } ;
          grid = 
          [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
              [| BK_bonus ; BK_bonus ; BK_bonus |] ;
              [| BK_bonus ; BK_bonus ; BK_empty |] |] 
        } ,
        2 ,
        2
      )
    )
  in
  assert_equals_result(BK_empty , l_res);
;;
test_fonc_brick_get();;
test_report();;

(*
===========================================================================================================
*)

(**
let brick_color(game , i , j : t_camlbrick * int * int) : t_camlbrick_color =
fonction qui prend en paramètre une game et les coordonnées d'une brique et renvoi
    la couleur de la brique.
     <ul>
      <li>Brique Block = Gris</li>
      <li>Brique Vide = Noir (couleur du fond d'écran)</li>
      <li>Brique Simple = Jaune</li>
      <li>Brique Bonus = Rouge</li>
      <li>Brique Double = Vert</li>
    </ul>

@author Nolan LAURIOUX
*)
let test_fonc_brick_color() : unit =

  test_reset_report() ;

  let test_fonc_brick_color_black() : unit = 
    let l_res : t_camlbrick_color t_test_result = 
      test_exec(
        brick_color , 
        "brick_color(
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
              brick_width = 40;
              brick_height = 20;
              paddle_init_width = 100;
              paddle_init_height = 20;
              time_speed = ref 20;
              } ; 
              grid = 
              [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
                  [| BK_bonus ; BK_bonus ; BK_bonus |] ;
                  [| BK_bonus ; BK_bonus ; BK_empty |] |] 
            }   
          } ,
          2 ,
          2 " , 
        (
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
    
              brick_width = 40;
              brick_height = 20;
    
              paddle_init_width = 100;
              paddle_init_height = 20;
    
              time_speed = ref 20;
              } ;
            grid = 
            [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
                [| BK_bonus ; BK_bonus ; BK_bonus |] ;
                [| BK_bonus ; BK_bonus ; BK_empty |] |] 
          } ,
          2 ,
          2
        )
      )
    in
    assert_equals_result(BLACK , l_res);
  in

  let test_fonc_brick_color_yellow() : unit = 
    let l_res : t_camlbrick_color t_test_result = 
      test_exec(
        brick_color , 
        "brick_color(
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
              brick_width = 40;
              brick_height = 20;
              paddle_init_width = 100;
              paddle_init_height = 20;
              time_speed = ref 20;
              } ; 
              grid = 
              [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
                  [| BK_bonus ; BK_bonus ; BK_bonus |] ;
                  [| BK_bonus ; BK_bonus ; BK_simple |] |] 
            }   
          } ,
          2 ,
          2 " , 
        (
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
      
              brick_width = 40;
              brick_height = 20;
      
              paddle_init_width = 100;
              paddle_init_height = 20;
      
              time_speed = ref 20;
              } ;
            grid = 
            [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
                [| BK_bonus ; BK_bonus ; BK_bonus |] ;
                [| BK_bonus ; BK_bonus ; BK_simple |] |] 
          } ,
          2 ,
          2
        )
      )
    in
    assert_equals_result(YELLOW , l_res);
  in

  let test_fonc_brick_color_green() : unit = 
    let l_res : t_camlbrick_color t_test_result = 
      test_exec(
        brick_color , 
        "brick_color(
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
              brick_width = 40;
              brick_height = 20;
              paddle_init_width = 100;
              paddle_init_height = 20;
              time_speed = ref 20;
              } ; 
              grid = 
              [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
                  [| BK_bonus ; BK_bonus ; BK_bonus |] ;
                  [| BK_bonus ; BK_bonus ; BK_double |] |] 
            }   
          } ,
          2 ,
          2 " , 
        (
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
      
              brick_width = 40;
              brick_height = 20;
      
              paddle_init_width = 100;
              paddle_init_height = 20;
      
              time_speed = ref 20;
              } ;
            grid = 
            [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
                [| BK_bonus ; BK_bonus ; BK_bonus |] ;
                [| BK_bonus ; BK_bonus ; BK_double |] |] 
          } ,
          2 ,
          2
        )
      )
    in
    assert_equals_result(GREEN , l_res);
  in

  let test_fonc_brick_color_gray() : unit = 
    let l_res : t_camlbrick_color t_test_result = 
      test_exec(
        brick_color , 
        "brick_color(
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
              brick_width = 40;
              brick_height = 20;
              paddle_init_width = 100;
              paddle_init_height = 20;
              time_speed = ref 20;
              } ; 
              grid = 
              [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
                  [| BK_bonus ; BK_bonus ; BK_bonus |] ;
                  [| BK_bonus ; BK_bonus ; BK_block |] |] 
            }   
          } ,
          2 ,
          2 " , 
        (
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
      
              brick_width = 40;
              brick_height = 20;
      
              paddle_init_width = 100;
              paddle_init_height = 20;
      
              time_speed = ref 20;
              } ;
            grid = 
            [|  [| BK_bonus ; BK_bonus ; BK_bonus |] ; 
                [| BK_bonus ; BK_bonus ; BK_bonus |] ;
                [| BK_bonus ; BK_bonus ; BK_block |] |] 
          } ,
          2 ,
          2
        )
      )
    in
    assert_equals_result(GRAY , l_res);
  in

  let test_fonc_brick_color_red() : unit = 
    let l_res : t_camlbrick_color t_test_result = 
      test_exec(
        brick_color , 
        "brick_color(
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
              brick_width = 40;
              brick_height = 20;
              paddle_init_width = 100;
              paddle_init_height = 20;
              time_speed = ref 20;
              } ; 
              grid = 
              [|  [| BK_simple ; BK_simple ; BK_simple |] ; 
                  [| BK_simple ; BK_simple ; BK_simple |] ;
                  [| BK_simple ; BK_simple ; BK_bonus |] |] 
            }   
          } ,
          2 ,
          2 " , 
        (
          {
            param = {
              world_width = 800;
              world_bricks_height = 600;
              world_empty_height = 200;
      
              brick_width = 40;
              brick_height = 20;
      
              paddle_init_width = 100;
              paddle_init_height = 20;
      
              time_speed = ref 20;
              } ;
            grid = 
            [|  [| BK_simple ; BK_simple ; BK_simple |] ; 
                [| BK_simple ; BK_simple ; BK_simple |] ;
                [| BK_simple ; BK_simple ; BK_bonus |] |] 
          } ,
          2 ,
          2
        )
      )
    in
    assert_equals_result(RED , l_res);
  in

  test_fonc_brick_color_black();
  test_fonc_brick_color_yellow();
  test_fonc_brick_color_green();
  test_fonc_brick_color_gray();
  test_fonc_brick_color_red();
  test_report();
  
;;
test_fonc_brick_color();;



  

