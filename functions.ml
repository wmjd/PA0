
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




