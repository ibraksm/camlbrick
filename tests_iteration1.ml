#mod_use "camlbrick.ml" ;;
open Camlbrick ;;

#mod_use "CPtest.ml" ;;
open CPtest ;;

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
test_fonc_brick_get();;
test_fonc_brick_hit();;
test_fonc_brick_color();;

(* Affiche le rapport de test *)
test_report() ;;