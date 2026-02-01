% Comparison test
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- Y > X.
?- max(5, 3, M).
