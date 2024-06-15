
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


% deletion Operations
% remove specific element
list_delete_element(Element, [Element|Tail], Tail).
list_delete_element(Element, [Head| Tail], [Head| ResultTail]):- list_delete_element(Element, Tail, ResultTail).

%remove first
list_shift([], []).
list_shift([H|T], T).

%remove last
list_pop([H|[]],[]).
list_pop([H|T], [H|R]):- list_pop(T, R).

% add Operations
% unshift = enqueue = add first
list_add_first(Element, [], [Element]).
list_add_first(Element, List, [Element|List]).

% add last (as of stack)
list_push_element(Element, [], [Element]).
list_push_element(Element, [H|T], [H|R]) :- list_push_element(Element, T, R).

% L = [1,2,3], [] =>  R = [1]
% L = [2, 3] , [] => R = [2, 1]
% L = [3], [2, 1] => R = [3, 2, 1]
% L = [] => R = [3, 2, 1]

list_reverse([], R, R).
list_reverse([H|T],RTemp, R):- list_add_first(H, RTemp, NewRTemp), list_reverse(T, NewRTemp, R).
list_reverse(List, R) :- list_reverse(List, [], R).