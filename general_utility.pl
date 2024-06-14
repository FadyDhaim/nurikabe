count_down(L, H) :-
   between(L, H, X),
   Y is H - (X - L),
   write(Y), nl.
   
count_up(L, H) :-
   between(L, H, X),
   write(X), nl.


count_to_10(10) :- write(10),nl.
count_to_10(X) :-
   write(X),nl,
   Y is X + 1,
   count_to_10(Y).
count_to_10 :-
    count_to_10(0).
