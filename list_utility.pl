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


% deletion Operation
list_delete_element(Element, [Element|Tail], Tail).
list_delete_element(Element, [Head| Tail], [Head| ResultTail]):- list_delete_element(Element, Tail, ResultTail).


% add Operations, unshift = enqueue = add first
list_unshift_element(Element, List, [Element|List]).

% add last (as of stack)
list_push_element(Element, [], [Element]).
list_push_element(Element, [H|T], [H|R]) :- list_push_element(Element, T, R).
