(*********************) 
(* CS 131 HOMEWORK 2 *)
(*********************)

 type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
 
(***************)
(*** Warm-up ***)
(***************)

(* GENERATE_GRAMMAR_RULE_LIST *
 * Takes two arguments: non-terminal and rule list. Recursively iterates through
 * input rule list to return any rules that correspond to the input non-
 * terminal. Curried function; when called without the non-terminal, this
 * becomes the function that resides in a Homework 2 style grammar. *)
let rec generate_grammar_rule_list rl nt = match rl with
	| []		-> []
	| (n, r)::t	->
		if 		n = nt
		then	[r] @ (generate_grammar_rule_list t nt)
		else	generate_grammar_rule_list t nt

let convert_grammar = function
	| (ss, rl)	-> (ss, (generate_grammar_rule_list rl))
	
(****************************************)
(*** Helper Functions for Parse_Prefix***)
(****************************************)

(* CHECK_RHS *
 * Recursively iterates through the rhs of a grammar rule to make sure each
 * element corresponds to a element of 'frag'. Mutual recursion begins if any
 * element is a non terminal, where 'matcher' will be called on that value
 * repeating the algorithm. *)
let rec check_rhs gram rhs acc d frag =
	if		rhs = []
	then	acc d frag
	else	match frag with
		| []	-> None
		| h::t	-> match rhs with
			| (N n) :: n_tail -> 
				(matcher gram n (check_rhs gram n_tail acc) d frag)
			| (T y) :: t_tail ->
				if		h = y 
				then	(check_rhs gram t_tail acc d t)
				else	None

(* CHECK_RULES *
 * Recursively iterates through rules for a given non-terminal to see if
 * any are valid. If one is found, it returns it. If not, it checks the next
 * rule. If none are found (called on an empty list) returns 'None'. *)
and check_rules gram nt rhs acc d frag =
	if		rhs = [] 
	then 	None 
	else 	match rhs with
		| h::t	-> match (check_rhs gram h acc (d@[(nt, h)]) frag) with
			| Some(a, b)	-> Some(a, b)
			| None 			-> (check_rules gram nt t acc d frag)
(* MATCHER *
 * Helper function used to initiate mutually recursive parse of input 'frag'. *)
and matcher gram nt acc d frag = (check_rules gram nt (gram nt) acc d frag)

(********************)
(*** Parse_Prefix ***)
(********************)

(* PARSE_PREFIX *
 * Behaves as described by Homework 2 spec. Implemented using 2 mutually
 * recurseive functions: check_rules and check_rhs. check_rules checks the rules
 * of a given non-terminal and check_rhs checks all the elements of a given
 * rule. *)
let rec parse_prefix (start_symbol, gram) acc frag =
    (matcher gram start_symbol acc [] frag)
