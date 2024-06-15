:- module(nurikabe_validator, [validate/0, one_sea/0]).

% Check if all sea cells (blue) are connected
one_sea :-
    findall((R,C), solve_cell(R, C, blue), BlueCells),
    all_connected(BlueCells).

% Check if there are any 2x2 blocks of black cells
no_2x2_blocks :-
    \+ (solve_cell(R, C, blue), solve_cell(R, C1, blue), C1 is C+1,
        solve_cell(R1, C, blue), solve_cell(R1, C1, blue), R1 is R+1).

% Check if each numbered cell is part of a white island of connected white cells
valid_islands :-
    \+ (fxd_cell(R, C, N), 
        \+ (island_size(R, C, N, [], Island), length(Island, L), L =:= N)).

% Check if two islands are connected
islands_not_connected :-
    findall((R,C), solve_cell(R, C, green), GreenCells),
    \+ (member((R, C), GreenCells), member((R1, C1), GreenCells), 
        R1 is R+1, C1 is C+1, 
        solve_cell(R, C1, green), solve_cell(R1, C, green)).

% Validate the solution
validate :-
    (one_sea, no_2x2_blocks, valid_islands, islands_not_connected ->
        writeln('The solution is valid');
        writeln('The solution is invalid')).

% Helper predicate to check connectivity of cells
all_connected([Cell|Cells]) :-
    reachable_cells([Cell], Cells, Reachable),
    length(Cells, L1),
    length(Reachable, L2),
    L1 =:= L2.

reachable_cells(Acc, [], Acc).
reachable_cells(Acc, [(R,C)|Cells], Reachable) :-
    findall((R2,C2), neighbor(R, C, R2, C2), Neighbors),
    subtract(Neighbors, Acc, NewNeighbors),
    append(Acc, NewNeighbors, NewAcc),
    reachable_cells(NewAcc, Cells, Reachable).

neighbor(R, C, R2, C2) :-
    (R2 is R + 1, C2 = C;
     R2 is R - 1, C2 = C;
     R2 = R, C2 is C + 1;
     R2 = R, C2 is C - 1),
    solve_cell(R2, C2, blue).

% Helper predicate to check the size of an island
island_size(R, C, N, Visited, Island) :-
    findall((R2, C2), (neighbor(R, C, R2, C2), \+ member((R2, C2), Visited), solve_cell(R2, C2, green)), Neighbors),
    append(Visited, [(R, C)], NewVisited),
    length(NewVisited, L), L =< N,
    (length(NewVisited, N) -> Island = NewVisited; 
    (island_size_neighbors(Neighbors, N, NewVisited, Island))).

island_size_neighbors([], _, Island, Island).
island_size_neighbors([(R,C)|Neighbors], N, Visited, Island) :-
    island_size(R, C, N, Visited, NewVisited),
    island_size_neighbors(Neighbors, N, NewVisited, Island).

