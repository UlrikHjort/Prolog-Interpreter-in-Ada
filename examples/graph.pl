% Graph Algorithms
% Path finding in directed and undirected graphs.

% Directed graph edges
edge(a, b).
edge(b, c).
edge(c, d).
edge(a, d).
edge(b, d).
edge(d, e).
edge(e, f).

% Path finding in directed graph
path(X, X, [X]).
path(X, Y, [X|Path]) :-
    edge(X, Z),
    path(Z, Y, Path).

% Path with cycle detection
path_no_cycle(X, X, _, [X]).
path_no_cycle(X, Y, Visited, [X|Path]) :-
    edge(X, Z),
    \+ member(Z, Visited),
    path_no_cycle(Z, Y, [Z|Visited], Path).

safe_path(X, Y, Path) :-
    path_no_cycle(X, Y, [X], Path).

% Undirected graph (bidirectional edges)
connected(X, Y) :- edge(X, Y).
connected(X, Y) :- edge(Y, X).

% Check if graph is connected (all nodes reachable from start)
reachable(X, X, _).
reachable(X, Y, Visited) :-
    connected(X, Z),
    \+ member(Z, Visited),
    reachable(Z, Y, [Z|Visited]).

% Member predicate
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% Example queries:
% ?- path(a, e, P).
% ?- safe_path(a, f, P).
