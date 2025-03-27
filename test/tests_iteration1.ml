#use "CPtest.ml";;
#use "iteration1.ml" ;;

(* Objectif de test : test unitaire de la fonction init_params*)

test_reset_report();;

let test_func_init_params_valid () : unit =
  let l_res : t_params t_test_result = test_exec (init_params, "init_params (30, 15, 60, 10)", (30, 15, 60, 10))
  in
  assert_equals_result_m ("init_params (30, 15, 60, 10) = t_params : {margin = 30 ; cell_size = 15 ; message_size = 60 ; grid_size = 10}", {margin = 30 ; cell_size = 15 ; message_size = 60 ; grid_size = 10}, l_res)
;;

let test_func_init_params_fail_margin () : unit =
  let l_res : t_params t_test_result = test_fail_exec (init_params, "init_params (-30, 15, 60, 10)", (-30, 15, 60, 10))
  in
  assert_failwith (l_res)
;;

let test_func_init_params_fail_cell_size () : unit =
  let l_res : t_params t_test_result = test_fail_exec (init_params, "init_params (30, -15, 60, 10)", (30, -15, 60, 10))
  in
  assert_failwith (l_res)
;;

let test_func_init_params_fail_message_size () : unit =
  let l_res : t_params t_test_result = test_fail_exec (init_params, "init_params (30, 15, -60, 10)", (30, 15, -60, 10))
  in
  assert_failwith (l_res)
;;

let test_func_init_params_fail_grid_size () : unit =
  let l_res : t_params t_test_result = test_fail_exec (init_params, "init_params (30, 15, 60, -10)", (30, 15, 60, -10))
  in
  assert_failwith (l_res)
;;

test_func_init_params_valid() ;;
test_func_init_params_fail_margin() ;;
test_func_init_params_fail_cell_size() ;;
test_func_init_params_fail_message_size() ;;
test_func_init_params_fail_grid_size() ;;

test_report();;