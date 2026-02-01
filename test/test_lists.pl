% List tests
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).
?- append([1, 2], [3, 4], X).
