list_sum([], 0) .
list_sum([H | T], Total) :-
	list_sum(T, Rest),
	Total is Rest + H.

list_prod([], 0).
list_prod([H | T], Prod) :-
	list_prod(T, Rest),
	Prod is Rest * H.

-(D, J, K) :-
	D is J - K.
-(D, J, K) :-
	D is K - J.

/(D, J, K) :-
	D is J / K.
/(D, J, K) :-
	D is K / J.
