% Knight's Tour Problem
% Find a path for a knight to visit every square on a chessboard exactly once.

% Board size
board_size(5).  % Use 5x5 for faster solutions

% Valid position on the board
valid_pos(X, Y) :-
    board_size(N),
    X >= 1, X =< N,
    Y >= 1, Y =< N.

% Knight's valid moves (L-shape: 2 squares in one direction, 1 in perpendicular)
knight_move(X1, Y1, X2, Y2) :-
    (DX = 2, DY = 1 ; DX = 2, DY = -1 ;
     DX = -2, DY = 1 ; DX = -2, DY = -1 ;
     DX = 1, DY = 2 ; DX = 1, DY = -2 ;
     DX = -1, DY = 2 ; DX = -1, DY = -2),
    X2 is X1 + DX,
    Y2 is Y1 + DY,
    valid_pos(X2, Y2).

% Knight's tour: find path visiting all squares
knight_tour(Path) :-
    board_size(N),
    Total is N * N,
    knight_tour_from(1, 1, [(1,1)], Total, Path).

knight_tour_from(_, _, Visited, Total, Path) :-
    length(Visited, Total), !,
    reverse(Visited, Path).
knight_tour_from(X, Y, Visited, Total, Path) :-
    knight_move(X, Y, X2, Y2),
    \+ member((X2, Y2), Visited),
    knight_tour_from(X2, Y2, [(X2, Y2)|Visited], Total, Path).

% Member check
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Length
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% Reverse
reverse(L, R) :- reverse_acc(L, [], R).
reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, R) :- reverse_acc(T, [H|Acc], R).

% Example query (warning: can be slow for large boards):
% ?- knight_tour(Path).
