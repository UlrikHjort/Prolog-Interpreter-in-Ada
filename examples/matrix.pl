% Matrix Operations
% Basic matrix operations represented as lists of lists.

% Example matrices
matrix_a([[1, 2], [3, 4]]).
matrix_b([[5, 6], [7, 8]]).
matrix_c([[1, 2, 3], [4, 5, 6], [7, 8, 9]]).

% Matrix dimensions
dimensions([], 0, 0).
dimensions([Row|_], Rows, Cols) :-
    length([Row|_], Rows),
    length(Row, Cols).

% Get element at position (1-indexed)
element_at(Matrix, Row, Col, Element) :-
    nth(Row, Matrix, RowList),
    nth(Col, RowList, Element).

nth(1, [H|_], H) :- !.
nth(N, [_|T], X) :-
    N > 1,
    N1 is N - 1,
    nth(N1, T, X).

% Matrix addition
matrix_add([], [], []).
matrix_add([R1|T1], [R2|T2], [R|T]) :-
    row_add(R1, R2, R),
    matrix_add(T1, T2, T).

row_add([], [], []).
row_add([H1|T1], [H2|T2], [H|T]) :-
    H is H1 + H2,
    row_add(T1, T2, T).

% Scalar multiplication
scalar_mult(_, [], []).
scalar_mult(S, [R|T], [SR|ST]) :-
    row_scalar_mult(S, R, SR),
    scalar_mult(S, T, ST).

row_scalar_mult(_, [], []).
row_scalar_mult(S, [H|T], [SH|ST]) :-
    SH is S * H,
    row_scalar_mult(S, T, ST).

% Matrix transpose
transpose([], []).
transpose([[]|_], []) :- !.
transpose(Matrix, [Row|Transposed]) :-
    first_column(Matrix, Row, Rest),
    transpose(Rest, Transposed).

first_column([], [], []).
first_column([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    first_column(Rows, Hs, Ts).

% Dot product of two vectors
dot_product([], [], 0).
dot_product([H1|T1], [H2|T2], P) :-
    dot_product(T1, T2, P1),
    P is H1 * H2 + P1.

% Matrix multiplication
matrix_mult(A, B, C) :-
    transpose(B, BT),
    mult_rows(A, BT, C).

mult_rows([], _, []).
mult_rows([Row|Rows], BT, [ResultRow|ResultRows]) :-
    mult_row_by_cols(Row, BT, ResultRow),
    mult_rows(Rows, BT, ResultRows).

mult_row_by_cols(_, [], []).
mult_row_by_cols(Row, [Col|Cols], [P|Products]) :-
    dot_product(Row, Col, P),
    mult_row_by_cols(Row, Cols, Products).

% Identity matrix of size N
identity(N, I) :- identity(N, 1, I).

identity(N, Current, []) :- Current > N, !.
identity(N, Current, [Row|Rest]) :-
    Current =< N,
    identity_row(N, Current, Row),
    Next is Current + 1,
    identity(N, Next, Rest).

identity_row(N, OnePos, Row) :- identity_row(N, 1, OnePos, Row).

identity_row(N, Current, _, []) :- Current > N, !.
identity_row(N, Current, OnePos, [1|Rest]) :-
    Current =< N, Current =:= OnePos, !,
    Next is Current + 1,
    identity_row(N, Next, OnePos, Rest).
identity_row(N, Current, OnePos, [0|Rest]) :-
    Current =< N,
    Next is Current + 1,
    identity_row(N, Next, OnePos, Rest).

% Length
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% Example queries:
% ?- matrix_add([[1, 2], [3, 4]], [[5, 6], [7, 8]], S).
% ?- transpose([[1, 2], [3, 4]], T).
% ?- matrix_mult([[1, 2], [3, 4]], [[5, 6], [7, 8]], P).
% ?- identity(3, I).
