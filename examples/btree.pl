% Binary Trees
% Trees represented as: nil (empty) or tree(Value, Left, Right)

% Check if tree is empty
empty(nil).

% Check if X is a member of the tree
tree_member(X, tree(X, _, _)).
tree_member(X, tree(_, Left, _)) :- tree_member(X, Left).
tree_member(X, tree(_, _, Right)) :- tree_member(X, Right).

% Count nodes in tree
tree_size(nil, 0).
tree_size(tree(_, Left, Right), Size) :-
    tree_size(Left, SL),
    tree_size(Right, SR),
    Size is SL + SR + 1.

% Calculate tree height
tree_height(nil, 0).
tree_height(tree(_, Left, Right), Height) :-
    tree_height(Left, HL),
    tree_height(Right, HR),
    max(HL, HR, MaxH),
    Height is MaxH + 1.

max(X, Y, X) :- X >= Y, !.
max(_, Y, Y).

% In-order traversal (Left, Root, Right)
inorder(nil, []).
inorder(tree(X, Left, Right), List) :-
    inorder(Left, LL),
    inorder(Right, RL),
    append(LL, [X|RL], List).

% Pre-order traversal (Root, Left, Right)
preorder(nil, []).
preorder(tree(X, Left, Right), [X|List]) :-
    preorder(Left, LL),
    preorder(Right, RL),
    append(LL, RL, List).

% Post-order traversal (Left, Right, Root)
postorder(nil, []).
postorder(tree(X, Left, Right), List) :-
    postorder(Left, LL),
    postorder(Right, RL),
    append(LL, RL, LR),
    append(LR, [X], List).

% Binary Search Tree: insert
bst_insert(X, nil, tree(X, nil, nil)).
bst_insert(X, tree(Y, Left, Right), tree(Y, NewLeft, Right)) :-
    X < Y, !,
    bst_insert(X, Left, NewLeft).
bst_insert(X, tree(Y, Left, Right), tree(Y, Left, NewRight)) :-
    X >= Y,
    bst_insert(X, Right, NewRight).

% Build BST from list
list_to_bst([], nil).
list_to_bst([H|T], Tree) :-
    list_to_bst(T, SubTree),
    bst_insert(H, SubTree, Tree).

% Append for lists
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Example tree: tree(5, tree(3, tree(1, nil, nil), tree(4, nil, nil)), tree(7, nil, nil))
% ?- tree_member(4, tree(5, tree(3, tree(1, nil, nil), tree(4, nil, nil)), tree(7, nil, nil))).
% ?- inorder(tree(5, tree(3, nil, nil), tree(7, nil, nil)), L).
