% Finite Automata
% Simulating finite state machines in Prolog.

% ============================================
% DETERMINISTIC FINITE AUTOMATON (DFA)
% ============================================
% DFA that accepts strings with an even number of 'a's

% States
state(q0).  % even number of a's (initial and accepting)
state(q1).  % odd number of a's

% Initial state
initial(q0).

% Accepting states
accepting(q0).

% Transitions: transition(FromState, Symbol, ToState)
transition(q0, a, q1).
transition(q0, b, q0).
transition(q1, a, q0).
transition(q1, b, q1).

% Accept a string (list of symbols)
dfa_accept(Input) :-
    initial(Start),
    dfa_run(Start, Input, Final),
    accepting(Final).

dfa_run(State, [], State).
dfa_run(State, [Symbol|Rest], Final) :-
    transition(State, Symbol, NextState),
    dfa_run(NextState, Rest, Final).

% ============================================
% NON-DETERMINISTIC FINITE AUTOMATON (NFA)
% ============================================
% NFA that accepts strings ending with 'ab'

% States for NFA
nfa_state(s0).
nfa_state(s1).
nfa_state(s2).

% Initial states (can have multiple)
nfa_initial(s0).

% Accepting states
nfa_accepting(s2).

% NFA transitions (can have multiple transitions for same state/symbol)
nfa_transition(s0, a, s0).
nfa_transition(s0, b, s0).
nfa_transition(s0, a, s1).
nfa_transition(s1, b, s2).

% NFA accepts if any path leads to accepting state
nfa_accept(Input) :-
    nfa_initial(Start),
    nfa_run(Start, Input, Final),
    nfa_accepting(Final).

nfa_run(State, [], State).
nfa_run(State, [Symbol|Rest], Final) :-
    nfa_transition(State, Symbol, NextState),
    nfa_run(NextState, Rest, Final).

% ============================================
% REGULAR EXPRESSION MATCHER
% ============================================
% Simple regex: match(Pattern, String)

% Empty pattern matches empty string
match([], []).

% Single character matches itself
match([C], [C]) :- atom(C).

% Concatenation
match(Pattern, String) :-
    append(P1, P2, Pattern),
    P1 \= [], P2 \= [],
    append(S1, S2, String),
    match(P1, S1),
    match(P2, S2).

% Star (zero or more occurrences)
match([star(P)], String) :-
    match_star(P, String).

match_star(_, []).
match_star(P, String) :-
    append(S1, S2, String),
    S1 \= [],
    match([P], S1),
    match_star(P, S2).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Example queries:
% ?- dfa_accept([a, b, a, b]).    % true (even a's)
% ?- dfa_accept([a, b, a]).       % false (odd a's)
% ?- nfa_accept([b, a, a, b]).    % true (ends with ab)
% ?- nfa_accept([a, b, a]).       % false (ends with a)
