read_rules(S,[]) :-
    at_end_of_stream(S).
read_rules(S,[L|R]) :- 
    not(at_end_of_stream(S)),
    read(S,L),
    read_rules(S,R).

map_rating(SERVICE,RATING,[SERVICE,RATING]).

rating_tennis_instructor(RATING_MAPPED):-
	repeat,
	writeln('What is the rating of the tennis instructor?'),
	read(RATING),
	(
    number(RATING),
    RATING >= 1,
    10 >= RATING
    ->
    map_rating(tennis_instructor,RATING,RATING_MAPPED),
    writef('Rating of the tenis instructor is %w', [RATING]),nl,nl, !
    ;
	writeln('The input should be a number between 1 and 10.'), fail
    ).

rating_equipment(RATING_MAPPED):-
	repeat,
	writeln('What is the rating of the equipment?'),
	read(RATING),
	(
    number(RATING),
    RATING >= 1,
    10 >= RATING
    ->
    map_rating(equipment,RATING,RATING_MAPPED),
    writef('Rating of the equipment is %w', [RATING]),nl,nl, !
    ;
	writeln('The input should be a number between 1 and 10.'), fail
    ).

ratings([R1,R2]):-
    rating_tennis_instructor(R1),
    rating_equipment(R2).

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


vaguness:- 
    open('rules.txt', read, R),
    read_rules(R, RULES),
    close(R),
    writeln('The created rules are:'),
    write(RULES),nl,
    open('curves.txt', read, C),
    read_rules(C, CURVES),
    close(C),
    writeln('The degree curves are:'),
    writeln(CURVES),nl,
    repeat,
    ratings(RATINGS),
    writef('Mapped ratings --> %w.', [RATINGS]),nl,nl,
    continue_or_end.