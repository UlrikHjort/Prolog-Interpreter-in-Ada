% Logic Puzzles

% ============================================
% WHO OWNS THE ZEBRA? (Einstein's Riddle - Simplified)
% ============================================
%
% There are 3 houses in a row.
% Each house has a different color, owner nationality, and pet.
%
% Clues:
% 1. The Englishman lives in the red house.
% 2. The Spaniard owns the dog.
% 3. The green house is immediately to the right of the white house.
% 4. The Japanese owns the zebra.
% 5. The green house owner drinks coffee.

% Simplified version with 3 houses
zebra_puzzle(Houses, ZebraOwner) :-
    Houses = [house(_, _, _), house(_, _, _), house(_, _, _)],

    % All colors are different
    member(house(red, _, _), Houses),
    member(house(green, _, _), Houses),
    member(house(white, _, _), Houses),

    % All nationalities are different
    member(house(_, english, _), Houses),
    member(house(_, spanish, _), Houses),
    member(house(_, japanese, _), Houses),

    % All pets are different
    member(house(_, _, dog), Houses),
    member(house(_, _, cat), Houses),
    member(house(_, _, zebra), Houses),

    % Clue 1: Englishman lives in red house
    member(house(red, english, _), Houses),

    % Clue 2: Spaniard owns dog
    member(house(_, spanish, dog), Houses),

    % Clue 3: Green house is right of white house
    right_of(house(green, _, _), house(white, _, _), Houses),

    % Clue 4: Japanese owns zebra
    member(house(_, japanese, zebra), Houses),

    % Find who owns the zebra
    member(house(_, ZebraOwner, zebra), Houses).

% X is immediately to the right of Y
right_of(X, Y, [Y, X | _]).
right_of(X, Y, [_|T]) :- right_of(X, Y, T).

% Member predicate
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% ============================================
% SEND + MORE = MONEY
% ============================================
% Classic cryptarithmetic puzzle

send_more_money(S, E, N, D, M, O, R, Y) :-
    digits([S, E, N, D, M, O, R, Y]),
    S > 0, M > 0,  % No leading zeros
    all_different([S, E, N, D, M, O, R, Y]),
    SEND is S*1000 + E*100 + N*10 + D,
    MORE is M*1000 + O*100 + R*10 + E,
    MONEY is M*10000 + O*1000 + N*100 + E*10 + Y,
    SEND + MORE =:= MONEY.

% Generate digit assignments
digits([]).
digits([H|T]) :-
    member(H, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
    digits(T).

% Check all elements are different
all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).

% Example queries:
% ?- zebra_puzzle(Houses, Owner).
% ?- send_more_money(S, E, N, D, M, O, R, Y).
