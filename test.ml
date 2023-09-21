open OUnit2
open Functions

let t_string name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:(fun x -> x))

let t_int name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:string_of_int)

let suite =
"suite">:::
 [

 t_int "max_4_5" (max 4 5) 5;

 t_int "fib_0" (fibonacci 0) 1;
 t_int "fib_4" (fibonacci 4) 5;

 ]
;;

run_test_tt_main suite
