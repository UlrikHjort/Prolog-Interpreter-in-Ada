% Length test
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.
?- length([a, b, c, d, e], N).
