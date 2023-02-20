negate(n(A),A).
negate(A,n(A)).

read_knowledge_base(S,[]) :-
    at_end_of_stream(S).
read_knowledge_base(S,[L|R]) :- 
    not(at_end_of_stream(S)),
    read(S,L),
    read_knowledge_base(S,R).

ask_q([[P1],[P2],[P3],[P4]]):-
    has_premium_sound_sistem(P3),
    car_price(P1),
    has_sunroof(P2),
    has_interface(P4).


to_pred(yes,SPredicate,SPredicate):-!.
to_pred(y,SPredicate,SPredicate):-!.
to_pred(no,SPredicate,n(SPredicate)):-!.
to_pred(n,SPredicate,n(SPredicate)):-!.
to_pred(Price,SPredicate,SPredicate):-
    Price >= 15000,
    !.
to_pred(_,SPredicate,n(SPredicate)):-!.

car_price(Predicate):-
	repeat,
	writeln('What is the car price?'),
	read(Price),
    nl,
	(
    number(Price),
    Price >= 1
    ->
	to_pred(Price, cost, Predicate),
    !;
	writeln('The input should be a non null number.'), fail
    ).

has_sunroof(Predicate):-
	repeat,
	writeln('Does the car have a sunroof?'),
	read(USER_ANS),
    nl,
	(
    member(USER_ANS, [yes, no, y, n])
    ->
	to_pred(USER_ANS, sunroof, Predicate),!
    ;
	writeln('The input should be yes,y,no or n.'), fail
    ).

has_premium_sound_sistem(Predicate):-
	repeat,
	writeln('Does the car have a premium sound system?'),
	read(USER_ANS),
    nl,
	(
    member(USER_ANS, [yes, no, y, n])
    ->
    to_pred(USER_ANS, premiumSoundSystem, Predicate),!
    ;
	writeln('The input should be yes,y,no or n.'), fail
    ).

has_interface(Predicate):-
	repeat,
	writeln('Does the car have an interface?'),
	read(USER_ANS),
    nl,
	(
    member(USER_ANS, [yes, no, y, n])
    ->
    to_pred(USER_ANS, interface, Predicate),!
    ;
	writeln('The input should be yes,y,no or n.'), fail
    ).

append_KB([HEAD|_], KB_ANS, R):-
    append(HEAD,KB_ANS,R).

continue_or_end :-
    writeln('Write end if you want to stop the program else continue.'),
    read(USER_ANS), nl,
    (
    USER_ANS = end 
    ->
    writef('%w typed. Program stoped.', [USER_ANS]), !
    ;
    USER_ANS = continue
    ->
    writef('%w typed. Program will continue.', [USER_ANS]), nl, fail
    ;
    writef('%w typed. Something else than "end" or "continue" was typed. Anyway the program will continue.', [USER_ANS]), nl, fail
    ).

back_forw_chaining:- 
    open('kb.txt', read, S),
    read_knowledge_base(S, KB_RULES), close(S),
    writeln('Knowledge based on rules.'),
    writeln(KB_RULES),nl,
    repeat,
    ask_q(KB_ANS),
    writeln('Knowledge based on user answers.'),
    writeln(KB_ANS),nl,
    writeln('ALL knowledge.'),
    append_KB(KB_RULES,KB_ANS, KB),
    writeln(KB),
    backward([n(highEndCar)], KB, R_back),nl,
    writef('Backward Chaining --> Determine whether a car is high-end or not : %w ' , [R_back]),nl,
    forward([highEndCar], KB,[], R_forw),
    writef('Forward Chaining --> Determine whether a car is high-end or not : %w ', [R_forw]),nl,
    continue_or_end.

backward([], _, 'yes'):-!.
backward([H|Goals], KB, R):-
    member(Clause, KB),
    negate(H,H_negated),
    member(H_negated, Clause),
	delete(Clause, H_negated, Clause_withouth_H_negated),
	append(Clause_withouth_H_negated, Goals, NewGoals), 
	backward(NewGoals, KB, R), !. 
backward(_, _, 'no'):- !. 


contained(List1, List2):-
    forall(member(X, List1), member(X, List2)).

get_negated_elements([], []).
get_negated_elements([n(H)|T], [H|NegT]):-
    get_negated_elements(T, NegT),!.
get_negated_elements([H|T], NegT):-
    H \= n(_),
    get_negated_elements(T, NegT),!.

forward(Goals, _, Solved, 'yes'):- 
    contained(Goals, Solved), !.

forward(Goals, KB, Solved, R):- 
    member(Clause, KB),
	member(Positive_atom, Clause),
    not(Positive_atom=n(_)),
	get_negated_elements(Clause, Clause_negated_atoms),
    contained(Clause_negated_atoms,Solved),
    not(contained([Positive_atom],Solved)),
    forward(Goals,KB,[Positive_atom|Solved],R), !.

forward(_, _, _, 'no'):- !.