% Simple Expert System
% A rule-based system for animal identification.

% Top-level query
identify(Animal) :-
    animal(Animal),
    write('The animal is: '),
    write(Animal), nl.

identify(unknown) :-
    write('Cannot identify the animal.'), nl.

% Animal rules based on characteristics
animal(cheetah) :-
    is_mammal,
    is_carnivore,
    has_tawny_color,
    has_dark_spots.

animal(tiger) :-
    is_mammal,
    is_carnivore,
    has_tawny_color,
    has_black_stripes.

animal(giraffe) :-
    is_mammal,
    is_ungulate,
    has_long_neck,
    has_long_legs,
    has_dark_spots.

animal(zebra) :-
    is_mammal,
    is_ungulate,
    has_black_stripes.

animal(ostrich) :-
    is_bird,
    cannot_fly,
    has_long_neck,
    has_long_legs.

animal(penguin) :-
    is_bird,
    cannot_fly,
    can_swim,
    is_black_and_white.

animal(albatross) :-
    is_bird,
    can_fly,
    is_good_flyer.

% Derived characteristics
is_mammal :- has_hair.
is_mammal :- gives_milk.

is_bird :- has_feathers.
is_bird :- can_fly, lays_eggs.

is_carnivore :- eats_meat.
is_carnivore :- has_pointed_teeth, has_claws, has_forward_eyes.

is_ungulate :- is_mammal, has_hooves.
is_ungulate :- is_mammal, chews_cud.

% Base facts (would normally be asked interactively)
% Uncomment the facts for the animal you want to identify:

% Facts for a cheetah:
has_hair.
eats_meat.
has_tawny_color.
has_dark_spots.

% Example query:
% ?- identify(X).
