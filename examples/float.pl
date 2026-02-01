% Floating Point Arithmetic Examples
% ==================================
% This example demonstrates floating-point number support.

% Circle calculations
circle_area(Radius, Area) :-
    Area is 3.14159265359 * Radius * Radius.

circle_circumference(Radius, Circ) :-
    Circ is 2.0 * 3.14159265359 * Radius.

% Temperature conversion
celsius_to_fahrenheit(C, F) :-
    F is C * 1.8 + 32.0.

fahrenheit_to_celsius(F, C) :-
    C is (F - 32.0) / 1.8.

% Quadratic formula: ax^2 + bx + c = 0
% Returns the two roots (assumes discriminant >= 0)
quadratic_roots(A, B, C, X1, X2) :-
    Discriminant is B * B - 4.0 * A * C,
    Discriminant >= 0,
    SqrtD is sqrt(Discriminant),
    X1 is (-B + SqrtD) / (2.0 * A),
    X2 is (-B - SqrtD) / (2.0 * A).

% Distance between two points
distance(X1, Y1, X2, Y2, D) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    D is sqrt(DX * DX + DY * DY).

% Compound interest: A = P(1 + r)^t
compound_interest(Principal, Rate, Time, Amount) :-
    Amount is Principal * ((1.0 + Rate) ** Time).

% Statistical mean of a list
sum_list([], 0.0).
sum_list([H|T], Sum) :-
    sum_list(T, Rest),
    Sum is H + Rest.

list_length([], 0).
list_length([_|T], N) :-
    list_length(T, N1),
    N is N1 + 1.

mean(List, Mean) :-
    sum_list(List, Sum),
    list_length(List, Len),
    Len > 0,
    Mean is Sum / Len.

% Example queries:
% ?- circle_area(5.0, A).
% ?- celsius_to_fahrenheit(100.0, F).
% ?- fahrenheit_to_celsius(212.0, C).
% ?- quadratic_roots(1.0, -5.0, 6.0, X1, X2).
% ?- distance(0.0, 0.0, 3.0, 4.0, D).
% ?- compound_interest(1000.0, 0.05, 10, A).
% ?- mean([1.0, 2.0, 3.0, 4.0, 5.0], M).
