(********************* 
 * CS 131 HOMEWORK 2 *
 *********************)

 type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
 
(*** Warm-up ***)

(* takes two arguments: non-terminal and rule list, returns list
 * of rules for that non terminal *)
let rec generate_grammar_rule_list rl nt = match rl with
	| []		-> []
	| (n, r)::t	->
		if 		n = nt
		then	[r] @ (generate_grammar_rule_list t nt)
		else	generate_grammar_rule_list t nt

let convert_grammar = function
	| (ss, rl)	-> (ss, (generate_grammar_rule_list rl))
