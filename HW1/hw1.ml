(********************************
	WARM UP FUNCTIONS 
********************************)

let subset a b = List.for_all (fun c -> List.mem c b) a

let proper_subset a b = (subset a b) && not (subset b a)

let equal_sets a b = (subset a b) && (subset b a)

let rec set_diff a b = match a with
        | [] 	-> []
        | h::t 	->
                if	List.mem h (b @ t)
                then	set_diff t b
                else	h :: (set_diff t b)

(********************************
	 POINT FUNCTIONS 
********************************)

let rec computed_fixed_point eq f x = 
	if 	eq x (f x)
	then	x
	else 	computed_fixed_point eq f (f x)

let rec compound f p x = 
	if 	p <= 0
	then	x
	else 	f (compound f (p - 1) x)

let rec computed_periodic_point eq f p x = 
	if 	eq x (compound f p x)
	then	x
	else	computed_periodic_point eq f p (f x)

(********************************
	BLIND ALLEY RULES 
********************************)

type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal

let pull a = match a with
        | N n -> n
	| T t -> t

let is_sub_terminal sr = match sr with
	| N _ -> false
	| T _ -> true

let is_terminal r =
	((List.length (snd r) = 1) && is_sub_terminal (List.hd (snd r)))

let rec strip_terminals l ret = match l with
	| [] 	-> ret
	| h::t 	->
		if 	is_sub_terminal h
		then	strip_terminals t ret
		else 	(strip_terminals t ((pull h)::ret))

let is_good g r = 
	if 	subset (strip_terminals (snd r) []) g
	then	true
	else	false

let rec add_rule g r = ((fst r)::g)

(*	| []	-> g
	| h::t	-> match h with
		| T _ -> add_rule g t
		| N n ->
			if	List.mem n g
			then	add_rule g t
			else	add_rule (n::g) t
*)
let rec resolve g r = match r with
	| [] 	-> g
	| h::t 	->
		if	((is_good g h) || (is_terminal h))
		then	resolve ((fst h)::g) t
		else	resolve g t
