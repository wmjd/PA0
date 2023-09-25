open OUnit2
open Functions

let t_string name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:(fun x -> x))

let t_int name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:string_of_int)

let t_alpha name value expected = name>::
  (fun _ -> assert_equal expected value )

let tree1 = Leaf
and tree2 = Node("", Leaf, Leaf)
and tree3 = Node("b", Node("a", Leaf, Leaf), Node("c", Leaf, Leaf))
and tree4 = Node("c", Node("b", Node("a", Leaf, Leaf), Leaf), Node("d", Leaf, Leaf))
and tree5 = Node("d", Node("c", Node("b", Node("a", Leaf, Leaf), Leaf), Leaf), Node("e", Leaf, Leaf))
and tree6 = Node("e", Node("d", Node("b", Node("a", Leaf, Leaf), Node("c", Leaf, Leaf)), Leaf), Node("f", Leaf, Leaf))

let suite =
"suite">:::
 [

 t_int "max_4_5" (max 4 5) 5;

 t_int "fib_0" (fibonacci 0) 1;
 t_int "fib_4" (fibonacci 4) 5;

 t_string "inorder_str_tree1" (inorder_str tree1) "";
 t_string "inorder_str_tree2" (inorder_str tree2) "";
 t_string "inorder_str_tree3" (inorder_str tree3) "abc";
 t_string "inorder_str_tree4" (inorder_str tree4) "abcd";
 t_string "inorder_str_tree5" (inorder_str tree5) "abcde";
 t_string "inorder_str_tree6" (inorder_str tree6) "abcdef";

 t_int "size_tree1" (size tree1) 0;
 t_int "size_tree2" (size tree2) 1;
 t_int "size_tree3" (size tree3) 3;
 t_int "size_tree4" (size tree4) 4;
 t_int "size_tree5" (size tree5) 5;
 t_int "size_tree6" (size tree6) 6;

 t_int "height_tree1" (height tree1) 0;
 t_int "height_tree2" (height tree2) 1;
 t_int "height_tree3" (height tree3) 2;
 t_int "height_tree4" (height tree4) 3;
 t_int "height_tree5" (height tree5) 4;
 t_int "height_tree6" (height tree6) 4;

 t_alpha "increment_all1" (increment_all [0;1;2;3;4]) [1;2;3;4;5];
 t_alpha "increment_all1" (increment_all []) [];

 t_alpha "long_strings1" (long_strings ["a"; "bb"; "ccc"; "dddd"] 1) ["bb"; "ccc"; "dddd"]; 
 t_alpha "long_strings2" (long_strings ["a"; "bb"; "ccc"; "dddd"] 4) []; 
 t_alpha "long_strings3" (long_strings ["a"; "bb"; "ccc"; "dddd"; "e"; "ffffff"] 4) ["ffffff"]; 
 t_alpha "long_strings4" (long_strings ["a"; "bb"; "ccc"; "dddd"] 0) ["a"; "bb"; "ccc"; "dddd"]; 
 t_alpha "long_strings5" (long_strings [] 0) []; 
 
 
 t_alpha "every_other1" (every_other [0; 1; 2; 3]) [0;2]; 
 t_alpha "every_other2" (every_other []) []; 
 t_alpha "every_other3" (every_other ["a"; "b"; "c"]) ["a"; "c"]; 

 t_alpha "sum_all1" (sum_all [[1;1;];[1;1]]) [2;2];
 t_alpha "sum_all2" (sum_all [[];[1;1]]) [0;2];
 t_alpha "sum_all3" (sum_all [[]]) [0];
 t_alpha "sum_all4" (sum_all [[-3;4];[1];[]]) [1;1;0];

 t_int "sum_of_squares1" (sum_of_squares [(1,1)]) 2;
 t_int "sum_of_squares2" (sum_of_squares [(1,1); (2,2)]) 10;
 t_int "sum_of_squares3" (sum_of_squares []) 0;
 t_int "sum_of_squares4" (sum_of_squares [(0,1); (4,4); (-2,-2)])  41;

 t_alpha "rem1" (remainders [4;6;10] 3) [(1,1);(2,0);(3,1)];
 t_alpha "rem2" (remainders [] 3) [];
 t_alpha "rem3" (remainders [1;-1;10] 2) [(0,1);(0,-1);(5,0)];

 t_alpha "mean1" (mean [1;1;1;1]) (Some 1);
 t_alpha "mean2" (mean []) (None);
 t_alpha "mean3" (mean [42]) (Some 42);
 t_alpha "mean4" (mean [42; 0]) (Some 21);

 t_alpha "list_max" (list_max [0;-1;1]) (Some 1);
 t_alpha "list_max" (list_max []) (None);
 t_alpha "list_max" (list_max [0]) (Some 0);
 t_alpha "list_max" (list_max []) (None);

 ]
;;

run_test_tt_main suite
