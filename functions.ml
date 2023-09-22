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

(* Lists & Parametric Polymorphism *)
let increment_all (ls : int list) : int list = 
	let rec iter ls acc = match ls with
		| [] -> acc
		| x::xs -> iter xs ((succ x)::acc)
	in List.rev (iter ls [])

let long_strings (ls : string list) (n : int) =
	let filter f ls = 
		let rec aux acc = function
			| [] -> List.rev acc
			| str::strs -> aux (if f str then str::acc else acc) strs 
		in aux [] ls
	in filter (fun s -> s |> String.length > n) ls

let every_other ls = 
	let rec aux ls acc = match ls with
		| [] -> List.rev acc
		| x::[] -> List.rev (x::acc) 
		| x::y::zs -> aux zs (x::acc)
	in aux ls []

let sum_all (ll : int list list) =
	let rec aux ll acc = match ll with
		| [] -> List.rev acc
		| ls::more -> aux more ((List.fold_left (fun running_total x -> x + running_total) 0 ls)::acc)
	in aux ll []

(* Tuples *)
let sum_of_squares (lp : (int * int) list) = (* this doesn't appear to be the usual  sum_of_squares *)
	let rec sumsq lp acc = match lp with
		| [] -> acc
		| (x,y)::more -> sumsq more (x*x + y*y + acc)
	in sumsq lp 0

let remainders ls d = 
	List.map (fun x -> (x/d, x mod d)) ls

(* Option Types *)
let mean ints = 
	let rec sum ints total =
		match ints with
		| [] -> total
		| x::xs -> sum xs (total+x)
	in if ints = [] then None else Some ((sum ints 0) / (List.length ints))
	
let list_max ls =
	if ls = [] then None else Some (List.fold_left (fun so_far x -> max so_far x) (List.hd ls) ls) 
