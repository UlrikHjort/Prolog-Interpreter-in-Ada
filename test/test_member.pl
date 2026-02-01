% Member test
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).
?- member(c, [a, b, c, d]).
