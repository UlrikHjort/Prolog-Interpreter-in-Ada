% Natural Language Parsing (DCG-style)
% Simple sentence parser using difference lists.
%
% Note: This is a manual implementation since our interpreter
% doesn't have DCG notation. In standard Prolog you'd use --> notation.

% Sentence = Noun Phrase + Verb Phrase
sentence(S, []) :- noun_phrase(S, R), verb_phrase(R, []).

% Noun Phrase = Determiner + Noun
noun_phrase(S, R) :- determiner(S, S1), noun(S1, R).

% Verb Phrase = Verb + Noun Phrase
verb_phrase(S, R) :- verb(S, S1), noun_phrase(S1, R).

% Verb Phrase = Verb (intransitive)
verb_phrase(S, R) :- verb_intrans(S, R).

% Determiners
determiner([the|R], R).
determiner([a|R], R).
determiner([an|R], R).

% Nouns
noun([cat|R], R).
noun([dog|R], R).
noun([mouse|R], R).
noun([man|R], R).
noun([woman|R], R).

% Transitive verbs
verb([chases|R], R).
verb([sees|R], R).
verb([likes|R], R).
verb([eats|R], R).

% Intransitive verbs
verb_intrans([runs|R], R).
verb_intrans([sleeps|R], R).
verb_intrans([barks|R], R).

% Check if a list of words forms a valid sentence
parse(Words) :- sentence(Words, []).

% Generate all valid sentences
generate_sentence(S) :- sentence(S, []).

% Example queries:
% ?- parse([the, cat, chases, a, mouse]).
% ?- parse([a, dog, barks]).
% ?- generate_sentence(S).
