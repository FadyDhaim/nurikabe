list_contains(_, []):- false.
list_contains(X, [H|T]):- X =:= H ; list_contains(X, T).

list_length([],0).
list_length([_|TAIL],N) :- list_length(TAIL,N1), N is N1 + 1.


% L1 = [1,2,3]
% L2 = [4,5,6]
% L3 = [] <== [1, 2, 3, 4, 5, 6]
% H = 1, T1 = [2, 3] => L3 = 1 -> [T3] <== [2, 3, 4, 5, 6]
% H = 2, T1 = [3] => 2 -> [T3] <== [3, 4, 5, 6]
% H = 3, T1 = [] => 3 -> [T3] <== [4, 5, 6]
% T3 = L2
list_concat([], L, L).
list_concat([H|T1], L2, [H|T3]) :- list_concat(T1, L2, T3).

list_delete_element(Element, Element, []).
list_delete_element(Element, [Element|Tail], Tail).
list_delete_element(Element, [Head| Tail], [Head| ResultTail]):- list_delete_element(Element, Tail, ResultTail).

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
