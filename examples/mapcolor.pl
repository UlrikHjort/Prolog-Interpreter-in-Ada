% Map Coloring Problem
% Color a map so that no two adjacent regions have the same color.

% Available colors
color(red).
color(green).
color(blue).
color(yellow).

% Adjacent regions cannot have the same color
valid_coloring(Regions) :-
    assign_colors(Regions),
    check_constraints(Regions).

% Assign colors to all regions
assign_colors([]).
assign_colors([region(_, Color)|Rest]) :-
    color(Color),
    assign_colors(Rest).

% Check that adjacent regions have different colors
check_constraints([]).
check_constraints([region(Name, Color)|Rest]) :-
    check_region(Name, Color, Rest),
    check_constraints(Rest).

check_region(_, _, []).
check_region(Name, Color, [region(Name2, Color2)|Rest]) :-
    (adjacent(Name, Name2) -> Color \= Color2 ; true),
    (adjacent(Name2, Name) -> Color \= Color2 ; true),
    check_region(Name, Color, Rest).

% Example: Simple map with 4 regions
% Define adjacencies
adjacent(a, b).
adjacent(a, c).
adjacent(b, c).
adjacent(b, d).
adjacent(c, d).

% Solve for the map
solve_map(Solution) :-
    Solution = [region(a, _), region(b, _), region(c, _), region(d, _)],
    valid_coloring(Solution).

% Example: Australian states
% WA, NT, SA, Q, NSW, V, T
adjacent_aus(wa, nt).
adjacent_aus(wa, sa).
adjacent_aus(nt, sa).
adjacent_aus(nt, q).
adjacent_aus(sa, q).
adjacent_aus(sa, nsw).
adjacent_aus(sa, v).
adjacent_aus(q, nsw).
adjacent_aus(nsw, v).

% Example queries:
% ?- solve_map(S).
