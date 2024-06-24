:- use_module(list_utility).

% Grid Definition
row(1). row(2). row(3). row(4). row(5). row(6). row(7). row(8). row(9).
column(1). column(2). column(3). column(4). column(5). column(6). column(7). column(8). column(9).
grid_size(9).

% Fixed Cells
fxd_cell(1, 2, 3). fxd_cell(1, 4, 6). fxd_cell(4, 1, 2). fxd_cell(4, 3, 4). fxd_cell(4, 5, 4). 
fxd_cell(6, 5, 1). fxd_cell(6, 7, 3). fxd_cell(6, 9, 2). fxd_cell(9, 6, 6). fxd_cell(9, 8, 4).

% Dynamic Predicates
:- dynamic solve_cell/3.
:- dynamic solve_cell_certain/3.
:- dynamic island/2.

% Cell Operations
set_blue_at(R, C) :- 
    unset_green_at(R, C),
    ( \+ solve_cell(R, C, blue) -> assertz(solve_cell(R, C, blue)) ; true ).

set_green_at(R, C) :- 
    unset_blue_at(R, C),
    ( \+ solve_cell(R, C, green) -> assertz(solve_cell(R, C, green)) ; true ).

set_blue_certain_at(R, C) :- set_blue_at(R, C), 
    (\+ solve_cell_certain(R, C, blue) -> assertz(solve_cell_certain(R, C, blue)) ; true).
set_green_certain_at(R, C) :- set_green_at(R, C), 
    (\+ solve_cell_certain(R, C, green) -> assertz(solve_cell_certain(R, C, green)) ; true).

unset_blue_at(R, C) :- (solve_cell(R, C, blue) -> retract(solve_cell(R, C, blue)); true).
unset_green_at(R, C) :- (solve_cell(R, C, green) -> retract(solve_cell(R, C, green)); true).

set_cells_with_certain_color([], _).
set_cells_with_certain_color([[R, C]|RestOfCells], Color) :-
    (Color == blue -> set_blue_certain_at(R, C); set_green_certain_at(R, C)),
    set_cells_with_certain_color(RestOfCells, Color).

set_cells_with_color([], _).
set_cells_with_color([[R, C]|RestOfCells], Color) :-
    (Color == blue -> set_blue_at(R, C); set_green_at(R, C)),
    set_cells_with_color(RestOfCells, Color).

% Island Setup
setup_islands:- 
    retractall(island(_,_)),
    setup_islands_helper.

setup_islands_helper:- 
    fxd_cell(R, C, _),
    connected_cells_to_cell(R, C, [], IslandCells),
    list_length(IslandCells, NumberOfIslandCells),
    assertz(island(IslandCells, NumberOfIslandCells)),
    fail.
setup_islands_helper.

% Cell Status Checks
is_green_cell(R, C) :- fxd_cell(R, C, _); solve_cell(R, C, green).
is_blue_cell(R, C) :- solve_cell(R, C, blue).
is_cell_of_color(R, C, Color) :- (Color == green -> is_green_cell(R, C); Color == blue -> is_blue_cell(R, C)).
get_cell_color(R, C, Color) :- (is_green_cell(R, C), Color = green; is_blue_cell(R, C), Color = blue).

empty_cell(R, C) :- \+is_green_cell(R, C), \+is_blue_cell(R, C).
empty_cells_of_cells([], FinalEmptyCells, FinalEmptyCells).
empty_cells_of_cells([[R, C]|T], InitialEmptyCells, FinalEmptyCells) :- 
    (empty_cell(R, C) -> list_add_first([R, C], InitialEmptyCells, UpdatedEmptyCells); UpdatedEmptyCells = InitialEmptyCells),
    empty_cells_of_cells(T, UpdatedEmptyCells, FinalEmptyCells).

are_cells_of_color([], _).
are_cells_of_color([[R, C]|T], Color) :- is_cell_of_color(R, C, Color), are_cells_of_color(T, Color).

are_empty_cells([]).
are_empty_cells([[R, C]|T]) :- empty_cell(R, C), are_empty_cells(T).

% Board Printing
print_board :- 
    nl,
    row(R),
    column(C),
    (fxd_cell(R, C, Number) -> 
        write(Number)
    ; (is_green_cell(R, C) -> 
        write('G')
    ; (is_blue_cell(R, C) -> 
        write('B')
    ; write('_')))),
    write(' '),
    grid_size(Size),
    (C =:= Size -> nl ; true),
    fail.
print_board :- nl.

% Adjacency and Connectivity
adjacent_cells_to_cell(Row, Column, AdjacentCells) :- 
    grid_size(Size),
    findall([R, C], (
            (R is Row - 1, C is Column, R > 0);
            (R is Row + 1, C is Column, R =< Size);
            (R is Row, C is Column - 1, C > 0); 
            (R is Row, C is Column + 1, C =< Size)
        ), AdjacentCells).

adjacent_cells_to_cell_of_the_same_color(Row, Column, AdjacentCells) :- 
    grid_size(Size),
    get_cell_color(Row, Column, Color),
    findall(
        [R, C],
        (
            (R is Row - 1, C is Column, R > 0, is_cell_of_color(R, C, Color));
            (R is Row + 1, C is Column, R =< Size, is_cell_of_color(R, C, Color));
            (R is Row, C is Column - 1, C > 0, is_cell_of_color(R, C, Color)); 
            (R is Row, C is Column + 1, C =< Size, is_cell_of_color(R, C, Color))
        ),
        AdjacentCells).

connected_cells_to_cell(Row, Column, Visited, FinalVisited) :- 
    \+ list_of_lists_contains_list([Row, Column], Visited),
    list_push_element([Row, Column], Visited, NewVisited),
    adjacent_cells_to_cell_of_the_same_color(Row, Column, AdjacentCells),
    process_adjacent_cells(AdjacentCells, NewVisited, FinalVisited).

process_adjacent_cells([], Visited, Visited).
process_adjacent_cells([[AdjacentCellRow, AdjacentCellColumn] | RestOfAdjacentCells], Visited, FinalVisited) :- 
    connected_cells_to_cell(AdjacentCellRow, AdjacentCellColumn, Visited, UpdatedVisited),
    process_adjacent_cells(RestOfAdjacentCells, UpdatedVisited, FinalVisited).
process_adjacent_cells([_ | RestOfAdjacentCells], Visited, FinalVisited) :- 
    process_adjacent_cells(RestOfAdjacentCells, Visited, FinalVisited).

% Validation
number_of_blue_cells(N) :- 
    findall([R, C], 
        is_blue_cell(R, C)
        ,BlueCells),
        list_length(BlueCells, N).

find_fixed_cell_in_island([[IslandCellRow, IslandCellColumn]|RestOfIslandCells], FixedCell):- 
    (fxd_cell(IslandCellRow, IslandCellColumn, _), FixedCell = [IslandCellRow, IslandCellColumn], !); 
    find_fixed_cell_in_island(RestOfIslandCells, FixedCell).

one_sea :- 
    solve_cell(R, C, blue),
    !, 
    connected_cells_to_cell(R, C, [], BlueCellsConnected),
    list_length(BlueCellsConnected, N1),
    number_of_blue_cells(N2),
    N1 =:= N2.

one_fixed_cell_in_island :- 
    island(IslandCells, _),
    findall([FixedCellRow, FixedCellColumn],
        (
        row(FixedCellRow),
        column(FixedCellColumn),
        list_of_lists_contains_list([FixedCellRow, FixedCellColumn], IslandCells),
        fxd_cell(FixedCellRow, FixedCellColumn, _)
    )
        ,FixedCellsInIsland),
        list_length(FixedCellsInIsland, NumberOfFixedCellsInIsland),
        NumberOfFixedCellsInIsland =:= 1.

island_number_equals_size :- 
    island(IslandCells, NumberOfIslandCells),
    find_fixed_cell_in_island(IslandCells, FixedCell),
    FixedCell = [FixedCellRow, FixedCellColumn],
    fxd_cell(FixedCellRow, FixedCellColumn, Number),
    Number =:= NumberOfIslandCells.

sea_upper_left_2_by_2(BlueCellRow, BlueCellColumn) :- 
    findall([R, C],
        (
            (R is BlueCellRow, C is BlueCellColumn - 1, C > 0, is_blue_cell(R, C));
            (R is BlueCellRow - 1, C is BlueCellColumn - 1, R > 0, C > 0, is_blue_cell(R, C));
            (R is BlueCellRow - 1, C is BlueCellColumn, R > 0, is_blue_cell(R, C))
        ),
        UpperLeftSeaCells),
    list_length(UpperLeftSeaCells, NumberOfUpperLeftSeaCells),
    NumberOfUpperLeftSeaCells =:= 3.

sea_upper_right_2_by_2(BlueCellRow, BlueCellColumn) :- 
    grid_size(Size),
    findall([R, C],
        (
            (R is BlueCellRow, C is BlueCellColumn + 1, C =< Size, is_blue_cell(R, C));
            (R is BlueCellRow - 1, C is BlueCellColumn + 1, R > 0, C =< Size, is_blue_cell(R, C));
            (R is BlueCellRow - 1, C is BlueCellColumn, R > 0, is_blue_cell(R, C))
        ),
        UpperRightSeaCells),
    list_length(UpperRightSeaCells, NumberOfUpperRightSeaCells),
    NumberOfUpperRightSeaCells =:= 3.

sea_lower_left_2_by_2(BlueCellRow, BlueCellColumn) :- 
    grid_size(Size),
    findall([R, C],
        (
            (R is BlueCellRow, C is BlueCellColumn - 1, C > 0, is_blue_cell(R, C));
            (R is BlueCellRow + 1, C is BlueCellColumn - 1, R =< Size, C > 0, is_blue_cell(R, C));
            (R is BlueCellRow + 1, C is BlueCellColumn, R =< Size, is_blue_cell(R, C))
        ),
        LowerLeftSeaCells),
    list_length(LowerLeftSeaCells, NumberOfLowerLeftSeaCells),
    NumberOfLowerLeftSeaCells =:= 3.

sea_lower_right_2_by_2(BlueCellRow, BlueCellColumn) :- 
    grid_size(Size),
    findall([R, C],
        (
            (R is BlueCellRow, C is BlueCellColumn + 1, C =< Size, is_blue_cell(R, C));
            (R is BlueCellRow + 1, C is BlueCellColumn + 1, R =< Size, C =< Size, is_blue_cell(R, C));
            (R is BlueCellRow + 1, C is BlueCellColumn, R =< Size, is_blue_cell(R, C))
        ),
        LowerRightSeaCells),
    list_length(LowerRightSeaCells, NumberOfLowerRightSeaCells),
    NumberOfLowerRightSeaCells =:= 3.

no_2_by_2_sea :- 
    findall([R, C],
        (
            is_blue_cell(R, C),
            (
        sea_upper_left_2_by_2(R, C);
        sea_upper_right_2_by_2(R, C);
        sea_lower_left_2_by_2(R, C);
        sea_lower_right_2_by_2(R, C)
        )
        ),
        Blocks2By2),
    list_length(Blocks2By2, NumberOf2By2Blocks),
    NumberOf2By2Blocks =:= 0.

% Starting Techniques
% 1. Island of 1
surround_1_celled_islands_with_sea :- 
    fxd_cell(R, C, N),
    N =:= 1,
    adjacent_cells_to_cell(R, C, AdjacentCells),
    set_cells_with_certain_color(AdjacentCells, blue),
    fail.
surround_1_celled_islands_with_sea.

% 2. Clues separated by one square
clues_separated_by_one_square :-
    fxd_cell(R1, C1, _),
    fxd_cell(R2, C2, _),
    (R1 =:= R2, abs(C1 - C2) =:= 2 -> C is (C1 + C2) // 2, set_blue_certain_at(R1, C);
     C1 =:= C2, abs(R1 - R2) =:= 2 -> R is (R1 + R2) // 2, set_blue_certain_at(R, C1)),
    fail.
clues_separated_by_one_square.

% 3. Diagonally adjacent clues
diagonally_adjacent_clues :-
    fxd_cell(R1, C1, _),
    fxd_cell(R2, C2, _),
    abs(R1 - R2) =:= 1,
    abs(C1 - C2) =:= 1,
    adjacent_cells_to_cell(R1, C1, AdjacentCells1),
    adjacent_cells_to_cell(R2, C2, AdjacentCells2),
    intersection(AdjacentCells1, AdjacentCells2, CommonCells),
    set_cells_with_certain_color(CommonCells, blue),
    fail.
diagonally_adjacent_clues.

% Basic Techniques
% 1. Surrounded square
fill_empty_cells_surrounded_with_sea :- 
    empty_cell(R, C),
    adjacent_cells_to_cell(R, C, AdjacentCells),
    are_cells_of_color(AdjacentCells, blue),
    set_blue_certain_at(R, C),
    fail.
fill_empty_cells_surrounded_with_sea.

% 2. Wall expansion
one_empty_cell_adjacent_to_a_blue_cell(R, C) :- 
    adjacent_cells_to_cell(R, C, AdjacentCells),
    empty_cells_of_cells(AdjacentCells, [], EmptyCells),
    list_length(EmptyCells, N),
    N =:= 1,
    EmptyCells = [[Row, Column]],
    set_blue_certain_at(Row, Column),
    one_empty_cell_adjacent_to_a_blue_cell(Row, Column).

wall_expansion_ie_one_empty_cell_adjacent_to_a_blue_cell :- 
    is_blue_cell(R, C),
    one_empty_cell_adjacent_to_a_blue_cell(R, C),
    fail.
wall_expansion_ie_one_empty_cell_adjacent_to_a_blue_cell.

% 3. Wall continuity
wall_continuity :-
    empty_cell(R, C),
    adjacent_cells_to_cell(R, C, AdjacentCells),
    are_cells_of_color(AdjacentCells, blue),
    set_blue_certain_at(R, C),
    fail.
wall_continuity.

% 4. Island expansion from a clue
one_way_island_expansion :- 
    fxd_cell(R, C, _),
    adjacent_cells_to_cell(R, C, AdjacentCells),
    empty_cells_of_cells(AdjacentCells, [], EmptyCells),
    list_length(EmptyCells, N),
    N =:= 1,
    EmptyCells = [[Row, Column]],
    set_green_certain_at(Row, Column),
    fail.
one_way_island_expansion.

% 5. Island expandable only in two directions
island_expandable_in_two_directions :-
    fxd_cell(R, C, 2),
    adjacent_cells_to_cell(R, C, AdjacentCells),
    empty_cells_of_cells(AdjacentCells, [], EmptyCells),
    list_length(EmptyCells, 2),
    [E1, E2] = EmptyCells,
    adjacent_cells_to_cell(E1, E1Adj),
    adjacent_cells_to_cell(E2, E2Adj),
    intersection(E1Adj, E2Adj, [[R1, C1]]),
    set_blue_certain_at(R1, C1),
    fail.
island_expandable_in_two_directions.

% 6. Hidden island expansion
hidden_island_expansion :-
    fxd_cell(R, C, N),
    N > 1,
    adjacent_cells_to_cell(R, C, AdjacentCells),
    empty_cells_of_cells(AdjacentCells, [], EmptyCells),
    list_length(EmptyCells, L),
    L < N,
    member([ER, EC], EmptyCells),
    set_green_certain_at(ER, EC),
    fail.
hidden_island_expansion.

% 7. Island continuity
island_continuity :-
    empty_cell(R, C),
    adjacent_cells_to_cell(R, C, AdjacentCells),
    are_cells_of_color(AdjacentCells, green),
    set_green_certain_at(R, C),
    fail.
island_continuity.

% 8. Surrounding a completed island
surround_completed_island :-
    island(IslandCells, NumberOfIslandCells),
    list_length(IslandCells, NumberOfIslandCells),
    set_cells_with_certain_color(IslandCells, green),
    adjacent_cells_to_cell(IslandCells, AdjacentCells),
    set_cells_with_certain_color(AdjacentCells, blue),
    fail.
surround_completed_island.

% 9. Avoiding wall area of 2x2
no_2_by_2_sea :- 
    findall([R, C],
        (
            is_blue_cell(R, C),
            (
        sea_upper_left_2_by_2(R, C);
        sea_upper_right_2_by_2(R, C);
        sea_lower_left_2_by_2(R, C);
        sea_lower_right_2_by_2(R, C)
        )
        ),
        Blocks2By2),
    list_length(Blocks2By2, NumberOf2By2Blocks),
    NumberOf2By2Blocks =:= 0.

% 10. Unreachable square
unreachable_square :-
    empty_cell(R, C),
    \+ (fxd_cell(FR, FC, _), connected_cells_to_cell(FR, FC, [], IslandCells), list_of_lists_contains_list([R, C], IslandCells)),
    set_blue_certain_at(R, C),
    fail.
unreachable_square.

% Advanced Techniques
% 1. Prevent 2x2 wall by island assumption
prevent_2x2_wall_by_island_assumption :-
    fxd_cell(R, C, 2),
    R1 is R + 1,
    empty_cell(R1, C),
    set_green_certain_at(R1, C),
    (no_2_by_2_sea -> true; set_blue_certain_at(R1, C), fail),
    fail.
prevent_2x2_wall_by_island_assumption.

% 2. Prevent 2x2 wall by wall assumption
prevent_2x2_wall_by_wall_assumption :-
    fxd_cell(R, C, 2),
    R1 is R + 1,
    empty_cell(R1, C),
    set_blue_certain_at(R1, C),
    (no_2_by_2_sea -> true; set_green_certain_at(R1, C), fail),
    fail.
prevent_2x2_wall_by_wall_assumption.

% Validation
validate :- 
    setup_islands, 
    one_sea, 
    one_fixed_cell_in_island, 
    island_number_equals_size, 
    no_2_by_2_sea,
    clues_separated_by_one_square,
    wall_continuity,
    island_expandable_in_two_directions,
    hidden_island_expansion,
    unreachable_square,
    prevent_2x2_wall_by_island_assumption,
    prevent_2x2_wall_by_wall_assumption.

ready_to_validate :- row(R), column(C), \+empty_cell(R, C).

% Initialization and Solving
initialize_game :- 
    retractall(solve_cell(_,_,_)),
    retractall(solve_cell_certain(_,_,_)),
    retractall(island(_,_)).

print_and_validate_static :- print_board, (validate -> writeln('Valid solution'); writeln('Invalid solution')).
start_static :- initialize_game, static_solve, print_and_validate_static. 
start_dynamic :- initialize_game, dynamic_solve.

:- set_prolog_flag(answer_write_options, [max_depth(0)]).

:- initialization(start_static).

static_solve :-
    set_green_at(1, 1),
    set_blue_at(1, 3),
    set_blue_at(1, 5),
    set_blue_at(1, 6),
    set_blue_at(1, 7),
    set_blue_at(1, 8),
    set_blue_at(1, 9),

    set_blue_at(2, 1),
    set_blue_at(2, 2),
    set_blue_at(2, 3),
    set_blue_at(2, 4),
    set_blue_at(2, 5),
    set_blue_at(2, 6),
    set_blue_at(2, 7),
    set_blue_at(2, 8),
    set_blue_at(2, 9),

    set_blue_at(3, 1),
    set_blue_at(3, 2),
    set_blue_at(3, 3),
    set_blue_at(3, 4),
    set_blue_at(3, 5),
    set_blue_at(3, 6),
    set_blue_at(3, 7),
    set_blue_at(3, 8),
    set_blue_at(3, 9),

    set_blue_at(4, 2),
    set_blue_at(4, 4),
    set_blue_at(4, 6),
    set_blue_at(4, 7),
    set_blue_at(4, 8),
    set_blue_at(4, 9),

    set_blue_at(5, 1),
    set_blue_at(5, 2),
    set_blue_at(5, 3),
    set_blue_at(5, 4),
    set_blue_at(5, 5),
    set_blue_at(5, 6),
    set_blue_at(5, 7),
    set_blue_at(5, 8),
    set_green_at(5, 9),

    set_blue_at(6, 1),
    set_green_at(6, 2),
    set_green_at(6, 3),
    set_blue_at(6, 4),
    set_blue_at(6, 6),
    set_blue_at(6, 8),

    set_blue_at(7, 1),
    set_blue_at(7, 2),
    set_blue_at(7, 3),
    set_blue_at(7, 4),
    set_blue_at(7, 5),
    set_green_at(7, 6),
    set_green_at(7, 7),
    set_blue_at(7, 8),
    set_blue_at(7, 9),

    set_blue_at(8, 1),
    set_green_at(8, 2),
    set_green_at(8, 3),
    set_green_at(8, 4),
    set_blue_at(8, 5),
    set_blue_at(8, 6),
    set_blue_at(8, 7),
    set_green_at(8, 8),
    set_green_at(8, 9),

    set_blue_at(9, 1),
    set_blue_at(9, 2),
    set_blue_at(9, 3),
    set_green_at(9, 4),
    set_green_at(9, 5),
    set_blue_at(9, 7),
    set_green_at(9, 9).