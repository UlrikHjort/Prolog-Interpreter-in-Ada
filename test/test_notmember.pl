% Not member test
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
?- member(z, [a, b, c]).
