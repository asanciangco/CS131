% Average time for plain_kenken with N=4 example:
%	~1.0	seconds
% Average time for kenken with N=4 example:	
%	~<.001	seconds

% ARITHMATIC CONSTRAINTS %

apply(+(Sum, []), Val, _) :- Val #= Sum.
apply(+(Sum, [H | Tail]), Val, T) :-
	get(H, T, E),
	NewVal = E + Val,
	apply(+(Sum, Tail), NewVal, T) .

apply(*(Prod, []), Val, _) :- Val #= Prod.
apply(*(Prod, [H | Tail]), Val, T) :-
	get(H, T, E),
	NewVal = E * Val,
	apply(*(Prod, Tail), NewVal, T) .

apply(-(Diff, J, K), T) :-
	get(J, T, A),
	get(K, T, B),
	A - B #= Diff.
apply(-(Diff, J, K), T) :-
	get(J, T, A),
	get(K, T, B),
	B - A #= Diff.
	
apply(/(Quot, J, K), T) :-
	get(J, T, A),
	get(K, T, B),
	A rem B #= 0,
	A / B #= Quot.
apply(/(Quot, J, K), T) :-
	get(J, T, A),
	get(K, T, B),
	B rem A #= 0,
	B / A #= Quot.

apply(+(S, [H|Tail]), T) :- apply(+(S, [H|Tail]), 0, T).
apply(*(P, [H|Tail]), T) :- apply(*(P, [H|Tail]), 1, T).

apply_all_constraints([], _) .
apply_all_constraints([Head | Tail], T) :-
	apply(Head, T),
	apply_all_constraints(Tail, T).
	
	
% ROW CONSTRAINTS %

unique_rows([]) .
unique_rows([H|T]) :-
	fd_all_different(H),
	unique_rows(T).


% COLUMN CONSTRAINTS %
	
compile_and_strip_heads([[H | T]], [H], [T]) .
compile_and_strip_heads([[H1 | T1] | Tail], Heads, Just_tails) :-
	compile_and_strip_heads(Tail, Rest_heads, Rest_tails),
	Heads = [H1 | Rest_heads],
	Just_tails = [T1 | Rest_tails].
	
unique_columns([[] | T]) .
unique_columns([[H|T]|Tt]) :-
	compile_and_strip_heads([[H|T]|Tt], Heads, Tails),
	fd_all_different(Heads),
	unique_columns(Tails).
	
unique_table(T) :-
	unique_columns(T),
	unique_rows(T).

	
% INITIALIZE TABLE %

init_table(N, T) :-
	length(T, N),
	init_rows(N, T).
	
init_rows(N, []) .
init_rows(N, [H | T]) :-
	length(H, N),
	fd_domain(H, 1, N),
	init_rows(N, T).
	

% GET ELEMENT %

get(R-C, T, E) :-
	nth1(R, T, Row),
	nth1(C, Row, E) .

	
% KEN KEN %

label([]) .
label([H | T]) :-
	fd_labeling(H),
	label(T).

kenken(N, C, T) :-
	init_table(N, T),
	apply_all_constraints(C, T),
	unique_table(T),
	label(T).
	
% SIMPLE STUFF %
% 	Below contains modified versions of the above functions.
% 	The same results are generated without using the fd_* predicates.

different(L) :-
	\+ (select(X,L,R), memberchk(X,R)).
	
unique_columns_plain([[] | T]) .
unique_columns_plain([[H|T]|Tt]) :-
	compile_and_strip_heads([[H|T]|Tt], Heads, Tails),
	different(Heads),
	unique_columns_plain(Tails).
	
unique_rows_plain([]) .
unique_rows_plain([H|T]) :-
	different(H),
	unique_rows_plain(T).
	
unique_table_plain(T) :-
	unique_columns_plain(T).
	%unique_rows_plain(T).

domain(L, L, _) .
domain(X, Lower, Upper) :-
	N is Lower + 1,
	N =< Upper,
	domain(X, N, Upper).
domain_list([], _, _) .
domain_list([H | T], Lower, Upper) :-
	domain(H, Lower, Upper),
	domain_list(T, Lower, Upper).
	
init_table_plain(N, T) :-
	length(T, N),
	init_rows_plain(N, T).
	
init_rows_plain(N, []) .
init_rows_plain(N, [H | T]) :-
	length(H, N),
	domain_list(H, 1, N),
	different(H),
	init_rows_plain(N, T).
	
% PLAIN ARITHMATIC CONSTRAINTS %

apply_plain(+(Sum, []), Val, _) :- Val =:= Sum.
apply_plain(+(Sum, [H | Tail]), Val, T) :-
	get(H, T, E),
	NewVal = E + Val,
	apply_plain(+(Sum, Tail), NewVal, T) .

apply_plain(*(Prod, []), Val, _) :- Val =:= Prod.
apply_plain(*(Prod, [H | Tail]), Val, T) :-
	get(H, T, E),
	NewVal = E * Val,
	apply_plain(*(Prod, Tail), NewVal, T) .

apply_plain(-(Diff, J, K), T) :-
	get(J, T, A),
	get(K, T, B),
	Diff is A - B.
apply_plain(-(Diff, J, K), T) :-
	get(J, T, A),
	get(K, T, B),
	Diff is B - A.
	
apply_plain(/(Quot, J, K), T) :-
	get(J, T, A),
	get(K, T, B),
	0 =:= A rem B,
	Quot is A // B.
apply_plain(/(Quot, J, K), T) :-
	get(J, T, A),
	get(K, T, B),
	B rem A =:= 0,
	Quot is B // A.

apply_plain(+(S, [H|Tail]), T) :- apply_plain(+(S, [H|Tail]), 0, T).
apply_plain(*(P, [H|Tail]), T) :- apply_plain(*(P, [H|Tail]), 1, T).

apply_all_constraints_plain([], _) .
apply_all_constraints_plain([Head | Tail], T) :-
	apply_plain(Head, T),
	apply_all_constraints_plain(Tail, T).
	
plain_kenken(N, C, T) :-
	init_table_plain(N, T),
	apply_all_constraints_plain(C, T),
	unique_table(T).
	
% TEST CASE %
 kenken_testcase(
  6,
  [
   +(11, [1-1, 2-1]),
   /(2, 1-2, 1-3),
   *(20, [1-4, 2-4]),
   *(6, [1-5, 1-6, 2-6, 3-6]),
   -(3, 2-2, 2-3),
   /(3, 2-5, 3-5),
   *(240, [3-1, 3-2, 4-1, 4-2]),
   *(6, [3-3, 3-4]),
   *(6, [4-3, 5-3]),
   +(7, [4-4, 5-4, 5-5]),
   *(30, [4-5, 4-6]),
   *(6, [5-1, 5-2]),
   +(9, [5-6, 6-6]),
   +(8, [6-1, 6-2, 6-3]),
   /(2, 6-4, 6-5)
  ]
).