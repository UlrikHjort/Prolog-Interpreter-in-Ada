% Sudoku Solver (4x4 version for tractability)
% A 4x4 Sudoku uses digits 1-4 and has 2x2 boxes.

% Solve a 4x4 Sudoku puzzle
% Board is represented as a list of 16 elements, row by row
% 0 represents an empty cell

sudoku(Puzzle, Solution) :-
    Solution = Puzzle,
    Puzzle = [S11, S12, S13, S14,
              S21, S22, S23, S24,
              S31, S32, S33, S34,
              S41, S42, S43, S44],

    % All cells must be 1-4
    digit(S11), digit(S12), digit(S13), digit(S14),
    digit(S21), digit(S22), digit(S23), digit(S24),
    digit(S31), digit(S32), digit(S33), digit(S34),
    digit(S41), digit(S42), digit(S43), digit(S44),

    % Row constraints
    all_different([S11, S12, S13, S14]),
    all_different([S21, S22, S23, S24]),
    all_different([S31, S32, S33, S34]),
    all_different([S41, S42, S43, S44]),

    % Column constraints
    all_different([S11, S21, S31, S41]),
    all_different([S12, S22, S32, S42]),
    all_different([S13, S23, S33, S43]),
    all_different([S14, S24, S34, S44]),

    % Box constraints (2x2 boxes)
    all_different([S11, S12, S21, S22]),
    all_different([S13, S14, S23, S24]),
    all_different([S31, S32, S41, S42]),
    all_different([S33, S34, S43, S44]).

% Digits 1-4
digit(1).
digit(2).
digit(3).
digit(4).

% All elements are different
all_different([]).
all_different([H|T]) :-
    \+ member(H, T),
    all_different(T).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Print a 4x4 board
print_board([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]) :-
    write(A), write(' '), write(B), write(' | '), write(C), write(' '), write(D), nl,
    write(E), write(' '), write(F), write(' | '), write(G), write(' '), write(H), nl,
    write('-----+-----'), nl,
    write(I), write(' '), write(J), write(' | '), write(K), write(' '), write(L), nl,
    write(M), write(' '), write(N), write(' | '), write(O), write(' '), write(P), nl.

% Example puzzle (0 = empty)
example_puzzle([
    1, 0, 0, 4,
    0, 0, 0, 0,
    0, 0, 0, 0,
    2, 0, 0, 3
]).

% Solve the example
solve_example :-
    example_puzzle(P),
    write('Puzzle:'), nl,
    print_board(P),
    nl,
    write('Solution:'), nl,
    sudoku(P, S),
    print_board(S).

% Example queries:
% ?- solve_example.
% ?- sudoku([1, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 3], S).
