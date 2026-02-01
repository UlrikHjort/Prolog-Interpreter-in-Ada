% Prime Numbers
% Predicates for working with prime numbers.

% Check if N is prime
is_prime(2) :- !.
is_prime(N) :-
    N > 2,
    N mod 2 =\= 0,
    Max is N - 1,
    \+ has_factor(N, 3, Max).

% Check if N has a factor between Low and High
has_factor(N, Low, _) :-
    Low * Low > N, !, fail.
has_factor(N, Low, _) :-
    N mod Low =:= 0, !.
has_factor(N, Low, High) :-
    Low < High,
    Low2 is Low + 2,
    has_factor(N, Low2, High).

% Generate primes up to N (Sieve-like approach)
primes_up_to(N, Primes) :-
    findall_primes(2, N, Primes).

findall_primes(Current, Max, []) :-
    Current > Max, !.
findall_primes(Current, Max, [Current|Rest]) :-
    Current =< Max,
    is_prime(Current), !,
    Next is Current + 1,
    findall_primes(Next, Max, Rest).
findall_primes(Current, Max, Rest) :-
    Current =< Max,
    Next is Current + 1,
    findall_primes(Next, Max, Rest).

% Nth prime number
nth_prime(1, 2) :- !.
nth_prime(N, P) :-
    N > 1,
    nth_prime_search(N, 3, P).

nth_prime_search(1, P, P) :- is_prime(P), !.
nth_prime_search(N, Current, P) :-
    N > 1,
    is_prime(Current), !,
    N1 is N - 1,
    Next is Current + 2,
    nth_prime_search(N1, Next, P).
nth_prime_search(N, Current, P) :-
    Next is Current + 2,
    nth_prime_search(N, Next, P).

% Prime factorization
prime_factors(1, []) :- !.
prime_factors(N, [F|Factors]) :-
    N > 1,
    smallest_factor(N, 2, F),
    N1 is N / F,
    prime_factors(N1, Factors).

smallest_factor(N, F, F) :-
    N mod F =:= 0, !.
smallest_factor(N, F, Result) :-
    F * F =< N,
    F1 is F + 1,
    smallest_factor(N, F1, Result).
smallest_factor(N, _, N).

% Greatest Common Divisor (Euclidean algorithm)
gcd(A, 0, A) :- A > 0, !.
gcd(A, B, G) :-
    B > 0,
    R is A mod B,
    gcd(B, R, G).

% Least Common Multiple
lcm(A, B, L) :-
    gcd(A, B, G),
    L is (A * B) / G.

% Example queries:
% ?- is_prime(17).
% ?- prime_factors(84, F).   % [2, 2, 3, 7]
% ?- gcd(48, 18, G).         % 6
% ?- lcm(4, 6, L).           % 12
