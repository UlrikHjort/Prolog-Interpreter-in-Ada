% Symbolic Differentiation
% Compute derivatives of mathematical expressions symbolically.

% Derivative of constant is 0
d(C, X, 0) :- number(C).
d(C, X, 0) :- atom(C), C \= X.

% Derivative of X with respect to X is 1
d(X, X, 1).

% Sum rule: d(F + G) = d(F) + d(G)
d(F + G, X, DF + DG) :-
    d(F, X, DF),
    d(G, X, DG).

% Difference rule: d(F - G) = d(F) - d(G)
d(F - G, X, DF - DG) :-
    d(F, X, DF),
    d(G, X, DG).

% Product rule: d(F * G) = F * d(G) + G * d(F)
d(F * G, X, F * DG + G * DF) :-
    d(F, X, DF),
    d(G, X, DG).

% Quotient rule: d(F / G) = (G * d(F) - F * d(G)) / (G * G)
d(F / G, X, (G * DF - F * DG) / (G * G)) :-
    d(F, X, DF),
    d(G, X, DG).

% Power rule: d(X^N) = N * X^(N-1) * d(X)
d(X ^ N, X, N * X ^ N1) :-
    number(N),
    N1 is N - 1.

% Chain rule for power: d(F^N) = N * F^(N-1) * d(F)
d(F ^ N, X, N * F ^ N1 * DF) :-
    number(N),
    N1 is N - 1,
    d(F, X, DF).

% Simplify expressions
simplify(X, X) :- atom(X), !.
simplify(N, N) :- number(N), !.
simplify(0 + X, S) :- !, simplify(X, S).
simplify(X + 0, S) :- !, simplify(X, S).
simplify(0 * _, 0) :- !.
simplify(_ * 0, 0) :- !.
simplify(1 * X, S) :- !, simplify(X, S).
simplify(X * 1, S) :- !, simplify(X, S).
simplify(X ^ 0, 1) :- !.
simplify(X ^ 1, S) :- !, simplify(X, S).
simplify(A + B, S) :-
    simplify(A, SA),
    simplify(B, SB),
    (number(SA), number(SB) -> S is SA + SB ; S = SA + SB).
simplify(A * B, S) :-
    simplify(A, SA),
    simplify(B, SB),
    (number(SA), number(SB) -> S is SA * SB ; S = SA * SB).
simplify(E, E).

% Derive and simplify
derive(Expr, Var, Result) :-
    d(Expr, Var, D),
    simplify(D, Result).

% Example queries:
% ?- d(x^2, x, D).              % D = 2*x^1
% ?- d(x^2 + 3*x + 5, x, D).    % D = 2*x^1*1 + (3*1 + x*0) + 0
% ?- derive(x^2, x, D).         % Simplified derivative
