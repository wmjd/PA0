(* The Basics *)
let rec fibonacci = function
	| 0 -> 1
	| 1 -> 1
	| n -> (fibonacci (n-1)) + (fibonacci (n-2))

	(* 
	trace out fibbonacci 4:
		fib 4
		(fib 3) + (fib 2)
		((fib 2) + (fib 1)) + ((fib 1) + (fib 0))
		(((fib 1) + (fib 0)) + 1) + (1 + 1)
		((1 + 1) + 1) + 2
		5
	*)

(* Datatypes *)
type btnode =
  | Leaf
  | Node of string * btnode * btnode

let rec inorder_str (bt : btnode) : string =
  match bt with
    | Leaf -> ""
    | Node(s, left, right) ->
      (inorder_str left) ^ s ^ (inorder_str right)
	(* 
	trace out inorder_str (Node("b", Node("a", Leaf, Leaf), Node("c", Leaf, Leaf))) 
		inor (Node("b", Node("a", Leaf, Leaf), Node("c", Leaf, Leaf)))
		(inor (Node("a", Leaf, Leaf))) ^ "b" ^ (inor (Node("c", Leaf, Leaf)))
		(inor Leaf) ^ "a" ^ (inor Leaf) ^ "b" ^ (inor Leaf) ^ "c" ^ (inor Leaf)
		"" ^ "a" ^ "" ^ "b" ^ "" ^ "c" ^ ""
		"abc"
	*)

let rec size (bt : btnode) : int =
  match bt with
	| Leaf -> 0
	| Node(s, left, right) ->
		1 + (size left) + (size right)

let rec height (bt : btnode) : int =
  match bt with
	| Leaf -> 0
	| Node(_, left, right) -> 1 + (max (height left) (height right))
