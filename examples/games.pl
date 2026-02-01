% Simple Games
% Tic-Tac-Toe and other simple games.

% ============================================
% TIC-TAC-TOE
% ============================================
% Board represented as a list of 9 elements
% [1, 2, 3, 4, 5, 6, 7, 8, 9] where numbers = empty, x/o = played

% Initial board
initial_board([1, 2, 3, 4, 5, 6, 7, 8, 9]).

% Display the board
display_board([A, B, C, D, E, F, G, H, I]) :-
    write(' '), write(A), write(' | '), write(B), write(' | '), write(C), nl,
    write('-----------'), nl,
    write(' '), write(D), write(' | '), write(E), write(' | '), write(F), nl,
    write('-----------'), nl,
    write(' '), write(G), write(' | '), write(H), write(' | '), write(I), nl.

% Make a move
move(Player, Pos, Board, NewBoard) :-
    nth1(Pos, Board, Pos),  % Position must be empty (still a number)
    replace(Pos, Player, Board, NewBoard).

% Replace element at position
replace(1, X, [_|T], [X|T]) :- !.
replace(N, X, [H|T], [H|R]) :-
    N > 1,
    N1 is N - 1,
    replace(N1, X, T, R).

nth1(1, [H|_], H) :- !.
nth1(N, [_|T], X) :-
    N > 1,
    N1 is N - 1,
    nth1(N1, T, X).

% Winning combinations
winning_line([A, B, C, _, _, _, _, _, _], P) :- A == P, B == P, C == P.
winning_line([_, _, _, A, B, C, _, _, _], P) :- A == P, B == P, C == P.
winning_line([_, _, _, _, _, _, A, B, C], P) :- A == P, B == P, C == P.
winning_line([A, _, _, B, _, _, C, _, _], P) :- A == P, B == P, C == P.
winning_line([_, A, _, _, B, _, _, C, _], P) :- A == P, B == P, C == P.
winning_line([_, _, A, _, _, B, _, _, C], P) :- A == P, B == P, C == P.
winning_line([A, _, _, _, B, _, _, _, C], P) :- A == P, B == P, C == P.
winning_line([_, _, A, _, B, _, C, _, _], P) :- A == P, B == P, C == P.

% Check for winner
winner(Board, Player) :- winning_line(Board, Player).

% Check for draw (no empty squares and no winner)
draw(Board) :-
    \+ member(1, Board),
    \+ member(2, Board),
    \+ member(3, Board),
    \+ member(4, Board),
    \+ member(5, Board),
    \+ member(6, Board),
    \+ member(7, Board),
    \+ member(8, Board),
    \+ member(9, Board),
    \+ winner(Board, _).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% ============================================
% NUMBER GUESSING GAME
% ============================================

% The secret number (change this or make it random)
secret_number(42).

guess_game :-
    write('Guess the number (1-100): '), nl,
    guess_loop(0).

guess_loop(Tries) :-
    Tries < 10,
    read(Guess),
    secret_number(Secret),
    check_guess(Guess, Secret, Tries).

check_guess(Guess, Secret, _) :-
    Guess =:= Secret, !,
    write('Correct! You win!'), nl.
check_guess(Guess, Secret, Tries) :-
    Guess < Secret, !,
    write('Too low!'), nl,
    NewTries is Tries + 1,
    guess_loop(NewTries).
check_guess(Guess, Secret, Tries) :-
    Guess > Secret,
    write('Too high!'), nl,
    NewTries is Tries + 1,
    guess_loop(NewTries).

% Example queries:
% ?- initial_board(B), display_board(B).
% ?- initial_board(B), move(x, 5, B, B2), move(o, 1, B2, B3), display_board(B3).
