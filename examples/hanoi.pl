% Towers of Hanoi
% Classic puzzle: move N disks from peg A to peg C using peg B as auxiliary.
% Rule: Never place a larger disk on a smaller one.

% hanoi(N, From, To, Aux) - Move N disks from From to To using Aux
hanoi(0, _, _, _) :- !.
hanoi(N, From, To, Aux) :-
    N > 0,
    N1 is N - 1,
    hanoi(N1, From, Aux, To),
    move(From, To),
    hanoi(N1, Aux, To, From).

% Print a move
move(From, To) :-
    write('Move disk from '),
    write(From),
    write(' to '),
    write(To),
    nl.

% Solve and count moves (2^N - 1)
hanoi_count(0, 0) :- !.
hanoi_count(N, Moves) :-
    N > 0,
    N1 is N - 1,
    hanoi_count(N1, M1),
    Moves is 2 * M1 + 1.

% Example: Solve for 3 disks
% ?- hanoi(3, left, right, center).
