% Family Relationships
% A classic Prolog example demonstrating facts and rules.

% Facts: parent(Parent, Child)
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).
parent(pat, mary).

% Gender facts
male(tom).
male(bob).
male(jim).
female(liz).
female(ann).
female(pat).
female(mary).

% Rules
father(X, Y) :- parent(X, Y), male(X).
mother(X, Y) :- parent(X, Y), female(X).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
grandfather(X, Z) :- grandparent(X, Z), male(X).
grandmother(X, Z) :- grandparent(X, Z), female(X).

sibling(X, Y) :- parent(Z, X), parent(Z, Y), X \= Y.
brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y) :- sibling(X, Y), female(X).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

descendant(X, Y) :- ancestor(Y, X).

uncle(X, Y) :- brother(X, Z), parent(Z, Y).
aunt(X, Y) :- sister(X, Z), parent(Z, Y).

cousin(X, Y) :- parent(PX, X), parent(PY, Y), sibling(PX, PY).

% Example queries:
% ?- father(tom, X).
% ?- grandparent(tom, X).
% ?- ancestor(tom, jim).
% ?- sibling(ann, X).
