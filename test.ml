open OUnit2
open Functions

let t_string name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:(fun x -> x))

let t_int name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:string_of_int)

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
 
 ]
;;

run_test_tt_main suite
