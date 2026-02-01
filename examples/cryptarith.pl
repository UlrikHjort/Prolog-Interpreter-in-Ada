% Cryptarithmetic Puzzles
% Solve puzzles where letters represent digits.

% ============================================
% SEND + MORE = MONEY
% ============================================

send_more_money([S, E, N, D, M, O, R, Y]) :-
    Digits = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    select(S, Digits, D1), S > 0,
    select(E, D1, D2),
    select(N, D2, D3),
    select(D, D3, D4),
    select(M, D4, D5), M > 0,
    select(O, D5, D6),
    select(R, D6, D7),
    select(Y, D7, _),
    SEND is S*1000 + E*100 + N*10 + D,
    MORE is M*1000 + O*100 + R*10 + E,
    MONEY is M*10000 + O*1000 + N*100 + E*10 + Y,
    SEND + MORE =:= MONEY.

% ============================================
% DONALD + GERALD = ROBERT
% ============================================

donald_gerald_robert([D, O, N, A, L, G, E, R, B, T]) :-
    Digits = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    select(D, Digits, D1), D > 0,
    select(O, D1, D2),
    select(N, D2, D3),
    select(A, D3, D4),
    select(L, D4, D5),
    select(G, D5, D6), G > 0,
    select(E, D6, D7),
    select(R, D7, D8), R > 0,
    select(B, D8, D9),
    select(T, D9, _),
    DONALD is D*100000 + O*10000 + N*1000 + A*100 + L*10 + D,
    GERALD is G*100000 + E*10000 + R*1000 + A*100 + L*10 + D,
    ROBERT is R*100000 + O*10000 + B*1000 + E*100 + R*10 + T,
    DONALD + GERALD =:= ROBERT.

% ============================================
% CROSS + ROADS = DANGER
% ============================================

cross_roads_danger([C, R, O, S, A, D, N, G, E]) :-
    Digits = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    select(C, Digits, D1), C > 0,
    select(R, D1, D2), R > 0,
    select(O, D2, D3),
    select(S, D3, D4),
    select(A, D4, D5),
    select(D, D5, D6), D > 0,
    select(N, D6, D7),
    select(G, D7, D8),
    select(E, D8, _),
    CROSS is C*10000 + R*1000 + O*100 + S*10 + S,
    ROADS is R*10000 + O*1000 + A*100 + D*10 + S,
    DANGER is D*100000 + A*10000 + N*1000 + G*100 + E*10 + R,
    CROSS + ROADS =:= DANGER.

% Select element from list
select(X, [X|T], T).
select(X, [H|T], [H|R]) :- select(X, T, R).

% ============================================
% Generic solver for A + B = C
% ============================================

% Usage: solve_add([A,B,C,D], [E,F,G,H], [I,J,K,L,M], Solution)
% where each list represents a number (most significant digit first)

solve_cryptarith(Word1, Word2, Word3) :-
    extract_letters(Word1, L1),
    extract_letters(Word2, L2),
    extract_letters(Word3, L3),
    append(L1, L2, L12),
    append(L12, L3, AllLetters),
    remove_dups(AllLetters, UniqueLetters),
    length(UniqueLetters, N),
    N =< 10,  % Can't have more than 10 unique digits
    assign_digits(UniqueLetters, [0,1,2,3,4,5,6,7,8,9]),
    word_to_num(Word1, N1), N1 > 0,
    word_to_num(Word2, N2), N2 > 0,
    word_to_num(Word3, N3), N3 > 0,
    N1 + N2 =:= N3.

extract_letters([], []).
extract_letters([H|T], [H|Rest]) :- atom(H), extract_letters(T, Rest).

remove_dups([], []).
remove_dups([H|T], R) :- member(H, T), !, remove_dups(T, R).
remove_dups([H|T], [H|R]) :- remove_dups(T, R).

assign_digits([], _).
assign_digits([Letter|Rest], Available) :-
    select(Digit, Available, Remaining),
    Letter = Digit,
    assign_digits(Rest, Remaining).

word_to_num([], 0).
word_to_num([D|Ds], N) :-
    word_to_num(Ds, N1),
    length(Ds, Len),
    Mult is 10 ** Len,
    N is D * Mult + N1.

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% Example queries:
% ?- send_more_money(Solution).
