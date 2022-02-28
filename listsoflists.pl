/* Georgios Tsiris, 1115201700173 */

:- set_flag(print_depth,1000).

cart_prod([], [[]]).
cart_prod([L|Ls], CP) :-
	cart_prod(Ls, Temp),
	distribute(L, Temp, CP).

distribute([], _, []).
distribute([X|Xs], Vs, Out) :-
	distribute1(X, Vs, Out1),
	distribute(Xs, Vs, Out2),
	append(Out1, Out2, Out).

distribute1(_, [], []).
distribute1(X, [V|Vs], [[X|V]|Out]) :-
	distribute1(X, Vs, Out).

matr_transp([[]|_], []).
matr_transp(M, [C|Temp]) :-
	first_column(M, C, M2),
	matr_transp(M2, Temp).

first_column([], [], []).
first_column([[X|Row]|M], [X|Xs], [Row|Rest]) :-
	first_column(M, Xs, Rest).

matr_mult([], _, []).
matr_mult([Row|M], M2, [Temp|Acc]) :-
	row_columns_mult(Row, M2, Temp),
	matr_mult(M, M2, Acc).

row_columns_mult(_, [[]|_], []).
row_columns_mult(Row, M, [A|Acc]) :-
	first_column(M, Col, Rest),
	element_A(Row, Col, A),
	row_columns_mult(Row, Rest, Acc).

element_A([], [], 0).
element_A([X|Row], [Y|Col], A) :-
	element_A(Row, Col, Acc),
	A is Acc + (X * Y).

matr_det([[X]], X).
matr_det(M, Det) :-
	first_column(M, Col, Rest),
	cut_rows(Rest, Rest, Col, Det).

cut_rows(_, [], [], Det) :-
	Det is 0.
cut_rows(M, [Row|Rest], [Y|Col], Det) :-
	M \= [[]],
	delete(Row, M, Sub_M),
	matr_det(Sub_M, Sub_Det),
	cut_rows2(M, Rest, Col, Acc),
	Det is (Acc + (Y * Sub_Det)).

cut_rows2(_, [], [], Det) :-
	Det is 0.
cut_rows2(M, [Row|Rest], [Y|Col], Det) :-
	M \= [[]],
	delete(Row, M, Sub_M),
	matr_det(Sub_M, Sub_Det),
	cut_rows(M, Rest, Col, Acc),
	Det is (Acc - (Y * Sub_Det)).