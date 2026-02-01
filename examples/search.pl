% Search Algorithms
% Classic AI search algorithms implemented in Prolog.

% ============================================
% DEPTH-FIRST SEARCH
% ============================================

% Generic DFS: dfs(Start, Goal, Path)
dfs(Start, Goal, Path) :-
    dfs_helper(Start, Goal, [Start], Path).

dfs_helper(Goal, Goal, Visited, Path) :-
    reverse(Visited, Path).
dfs_helper(Current, Goal, Visited, Path) :-
    edge(Current, Next),
    \+ member(Next, Visited),
    dfs_helper(Next, Goal, [Next|Visited], Path).

% ============================================
% BREADTH-FIRST SEARCH
% ============================================

% Generic BFS: bfs(Start, Goal, Path)
bfs(Start, Goal, Path) :-
    bfs_helper([[Start]], Goal, Path).

bfs_helper([[Goal|Rest]|_], Goal, Path) :-
    reverse([Goal|Rest], Path).
bfs_helper([[Current|Path]|Paths], Goal, FinalPath) :-
    findall([Next, Current|Path],
            (edge(Current, Next), \+ member(Next, [Current|Path])),
            NewPaths),
    append(Paths, NewPaths, AllPaths),
    bfs_helper(AllPaths, Goal, FinalPath).

% Manual findall
findall(Template, Goal, List) :-
    collect_all(Template, Goal, [], List).

collect_all(Template, Goal, Acc, List) :-
    copy_term((Template, Goal), (T, G)),
    G,
    \+ member(T, Acc), !,
    collect_all(Template, Goal, [T|Acc], List).
collect_all(_, _, List, Reversed) :-
    reverse(List, Reversed).

% ============================================
% ITERATIVE DEEPENING
% ============================================

% iddfs(Start, Goal, Path)
iddfs(Start, Goal, Path) :-
    iddfs_helper(Start, Goal, 1, Path).

iddfs_helper(Start, Goal, Depth, Path) :-
    dfs_limited(Start, Goal, Depth, [Start], Path).
iddfs_helper(Start, Goal, Depth, Path) :-
    Depth1 is Depth + 1,
    Depth1 =< 20,  % Prevent infinite loops
    iddfs_helper(Start, Goal, Depth1, Path).

dfs_limited(Goal, Goal, _, Visited, Path) :-
    reverse(Visited, Path).
dfs_limited(Current, Goal, Depth, Visited, Path) :-
    Depth > 0,
    edge(Current, Next),
    \+ member(Next, Visited),
    Depth1 is Depth - 1,
    dfs_limited(Next, Goal, Depth1, [Next|Visited], Path).

% ============================================
% SAMPLE GRAPH
% ============================================

edge(a, b).
edge(a, c).
edge(b, d).
edge(b, e).
edge(c, f).
edge(d, g).
edge(e, g).
edge(f, g).

% Helper predicates
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

reverse(L, R) :- reverse_acc(L, [], R).
reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, R) :- reverse_acc(T, [H|Acc], R).

append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

copy_term(X, X).  % Simplified - real Prolog has proper copy_term

% Example queries:
% ?- dfs(a, g, Path).
% ?- bfs(a, g, Path).
% ?- iddfs(a, g, Path).
