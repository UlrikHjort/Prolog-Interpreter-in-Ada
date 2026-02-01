% Propositional Logic
% Evaluating and manipulating logical expressions.

% Truth values
bool(true).
bool(false).

% Negation
not(true, false).
not(false, true).

% Conjunction (AND)
and(true, true, true).
and(true, false, false).
and(false, true, false).
and(false, false, false).

% Disjunction (OR)
or(true, true, true).
or(true, false, true).
or(false, true, true).
or(false, false, false).

% Implication (IF-THEN)
implies(true, true, true).
implies(true, false, false).
implies(false, true, true).
implies(false, false, true).

% Biconditional (IFF)
iff(true, true, true).
iff(true, false, false).
iff(false, true, false).
iff(false, false, true).

% XOR
xor(true, true, false).
xor(true, false, true).
xor(false, true, true).
xor(false, false, false).

% Evaluate expression with variable bindings
% Expressions: true, false, var(Name), not(E), and(E1, E2), or(E1, E2)
eval(true, _, true).
eval(false, _, false).
eval(var(Name), Bindings, Value) :-
    member((Name, Value), Bindings).
eval(not(E), Bindings, Value) :-
    eval(E, Bindings, V),
    not(V, Value).
eval(and(E1, E2), Bindings, Value) :-
    eval(E1, Bindings, V1),
    eval(E2, Bindings, V2),
    and(V1, V2, Value).
eval(or(E1, E2), Bindings, Value) :-
    eval(E1, Bindings, V1),
    eval(E2, Bindings, V2),
    or(V1, V2, Value).
eval(implies(E1, E2), Bindings, Value) :-
    eval(E1, Bindings, V1),
    eval(E2, Bindings, V2),
    implies(V1, V2, Value).

% Check if expression is a tautology (true for all assignments)
tautology(Expr, Vars) :-
    \+ counterexample(Expr, Vars, _).

counterexample(Expr, Vars, Bindings) :-
    all_assignments(Vars, Bindings),
    eval(Expr, Bindings, false).

% Generate all possible truth assignments
all_assignments([], []).
all_assignments([V|Vs], [(V, Val)|Rest]) :-
    bool(Val),
    all_assignments(Vs, Rest).

% Check satisfiability
satisfiable(Expr, Vars, Bindings) :-
    all_assignments(Vars, Bindings),
    eval(Expr, Bindings, true).

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Example queries:
% ?- eval(and(var(p), var(q)), [(p, true), (q, false)], R).
% ?- tautology(or(var(p), not(var(p))), [p]).  % p OR NOT p
% ?- satisfiable(and(var(p), var(q)), [p, q], B).
