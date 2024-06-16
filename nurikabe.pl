:- use_module(list_utility).
row(1).
row(2).
row(3).
row(4).
row(5).
row(6).
row(7).
row(8).
row(9).
column(1).
column(2).
column(3).
column(4).
column(5).
column(6).
column(7).
column(8).
column(9).
grid_size(9).

fxd_cell(1, 2, 3).
fxd_cell(1, 4, 6).
fxd_cell(4, 1, 2).
fxd_cell(4, 3, 4).
fxd_cell(4, 5, 4).
fxd_cell(6, 5, 1).
fxd_cell(6, 7, 3).
fxd_cell(6, 9, 2).
fxd_cell(9, 6, 6).
fxd_cell(9, 8, 4).

:- dynamic solve_cell/3.
:- dynamic island/2. %مصفوفة خلايا الجزيرة وعدد خلايا الجزيرة

is_green_cell(Row, Column) :- fxd_cell(Row, Column, _); solve_cell(Row, Column, green).
is_blue_cell(Row, Column) :- solve_cell(Row, Column, blue).
is_cell_of_color(Row, Column, Color) :- (Color == green -> is_green_cell(Row, Column); Color == blue -> is_blue_cell(Row, Column)).
get_cell_color(Row, Column, Color) :- (is_green_cell(Row, Column), Color = green; is_blue_cell(Row, Column), Color = blue).

% طباعة الرقعة
print_board :-
    nl,
    row(R), column(C),
    (fxd_cell(R, C, Number) ->
        write(Number)
    ; solve_cell(R, C, Color) ->
        (Color == green -> write('G') ; write('B'))
    ; write('_')),
    write(' '),
    grid_size(Size),
    (C =:= Size -> nl ; true),
    fail.
print_board :- nl.


% ايجاد الخلايا المجاورة لخلية من نفس اللون
adjacent_cells_to_cell(Row, Column, AdjacentCells) :-
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


% ايجاد مجموعة الخلايا الموصلة ان كان جزيرة او بحر
connected_cells_to_cell(Row, Column, Visited, FinalVisited) :-
    \+ list_of_lists_contains_list([Row, Column], Visited),
    list_push_element([Row, Column], Visited, NewVisited),
    adjacent_cells_to_cell(Row, Column, AdjacentCells),
    process_adjacent_cells(AdjacentCells, NewVisited, FinalVisited).

process_adjacent_cells([], Visited, Visited).
process_adjacent_cells([[AdjacentCellRow, AdjacentCellColumn] | RestOfAdjacentCells], Visited, FinalVisited) :-
    connected_cells_to_cell(AdjacentCellRow, AdjacentCellColumn, Visited, UpdatedVisited),
    process_adjacent_cells(RestOfAdjacentCells, UpdatedVisited, FinalVisited).

%بحال ازا الخلية يلي بالاستدعاء يلي فوق زرناها قبل ف برجع فولس ومنيجي لهون لنكفي عالباقي
process_adjacent_cells([_ | RestOfAdjacentCells], Visited, FinalVisited) :-
    process_adjacent_cells(RestOfAdjacentCells, Visited, FinalVisited).


store_islands:-
    findall([FixedCellRow, FixedCellColumn], 
        (
            fxd_cell(FixedCellRow, FixedCellColumn, _),
            connected_cells_to_cell(FixedCellRow, FixedCellColumn, [], IslandCells),
    list_length(IslandCells, NumberOfIslandCells),
    assertz(island(IslandCells, NumberOfIslandCells))), _).


number_of_blue_cells(N) :-
    findall([R, C], 
        is_blue_cell(R, C)
        ,BlueCells),
        list_length(BlueCells, N).

find_fixed_cell_in_island([[IslandCellRow, IslandCellColumn]|RestOfIslandCells], FixedCell):-
    (fxd_cell(IslandCellRow, IslandCellColumn, _), FixedCell = [IslandCellRow, IslandCellColumn], !); find_fixed_cell_in_island(RestOfIslandCells, FixedCell).

%شروط التحقق الاربعة
one_sea :-
    solve_cell(R, C, blue),
    !, %لا تجرب خلية زرقا تانية
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
    



no_2_by_2_sea_upper_left(BlueCellRow, BlueCellColumn) :-
    findall([R, C],
        (
        (R is BlueCellRow, C is BlueCellColumn - 1, C > 0, is_blue_cell(R, C));
        (R is BlueCellRow - 1, C is BlueCellColumn - 1, R > 0, C > 0, is_blue_cell(R, C));
        (R is BlueCellRow - 1, C is BlueCellColumn, R > 0, is_blue_cell(R, C))
        ),
        UpperLeftSeaCells), list_length(UpperLeftSeaCells, NumberOfUpperLeftSeaCells), NumberOfUpperLeftSeaCells =\= 3.
no_2_by_2_sea_upper_right(BlueCellRow, BlueCellColumn) :-
grid_size(Size),
    findall([R, C],
        (
        (R is BlueCellRow, C is BlueCellColumn + 1, C =< Size, is_blue_cell(R, C));
        (R is BlueCellRow - 1, C is BlueCellColumn + 1, R > 0, C =< Size, is_blue_cell(R, C));
        (R is BlueCellRow - 1, C is BlueCellColumn, R > 0, is_blue_cell(R, C))
        ),
        UpperRightSeaCells), list_length(UpperRightSeaCells, NumberOfUpperRightSeaCells), NumberOfUpperRightSeaCells =\= 3.

no_2_by_2_sea_lower_left(BlueCellRow, BlueCellColumn) :-
    grid_size(Size),
    findall([R, C],
        (
        (R is BlueCellRow, C is BlueCellColumn - 1, C > 0 , is_blue_cell(R, C));
        (R is BlueCellRow + 1, C is BlueCellColumn - 1,  R =< Size, C > 0, is_blue_cell(R, C));
        (R is BlueCellRow + 1, C is BlueCellColumn, R =< Size, is_blue_cell(R, C))
        ),
        LowerLeftSeaCells), list_length(LowerLeftSeaCells, NumberOfLowerLeftSeaCells), NumberOfLowerLeftSeaCells =\= 3.

no_2_by_2_sea_lower_right(BlueCellRow, BlueCellColumn) :-
    grid_size(Size),
    findall([R, C],
        (
        (R is BlueCellRow, C is BlueCellColumn + 1, C =< Size , is_blue_cell(R, C));
        (R is BlueCellRow + 1, C is BlueCellColumn + 1,  R =< Size, C =< Size, is_blue_cell(R, C));
        (R is BlueCellRow + 1, C is BlueCellColumn, R =< Size, is_blue_cell(R, C))
        ),
        LowerRightSeaCells), list_length(LowerRightSeaCells, NumberOfLowerRightSeaCells), NumberOfLowerRightSeaCells =\= 3.

no_2_by_2_sea :-
    findall([R, C],
        (
    is_blue_cell(R, C),
    no_2_by_2_sea_upper_left(R, C),
    no_2_by_2_sea_upper_right(R, C),
    no_2_by_2_sea_lower_left(R, C),
    no_2_by_2_sea_lower_right(R, C)
    ),Blocks2By2), list_length(Blocks2By2, NumberOf2By2Blocks), NumberOf2By2Blocks =:= 0.









validate :- store_islands, one_sea, one_fixed_cell_in_island, island_number_equals_size, no_2_by_2_sea.


print_and_validate :- print_board, (validate -> writeln('Valid solution'); writeln('Invalid solution')).


start_static :- initialize_game, static_solve, print_and_validate. 
:- set_prolog_flag(answer_write_options, [max_depth(0)]).
:- initialization(start_static).

initialize_game :- 
    retractall(solve_cell(_,_,_)),
    retractall(island(_,_)).
    
static_solve:-
assertz(solve_cell(1, 1, green)),
assertz(solve_cell(1, 3, blue)),
assertz(solve_cell(1, 5, blue)),
assertz(solve_cell(1, 6, blue)),
assertz(solve_cell(1, 7, blue)),
assertz(solve_cell(1, 8, blue)),
assertz(solve_cell(1, 9, blue)),

assertz(solve_cell(2, 1, blue)),
assertz(solve_cell(2, 2, green)),
assertz(solve_cell(2, 3, blue)),
assertz(solve_cell(2, 4, green)),
assertz(solve_cell(2, 5, green)),
assertz(solve_cell(2, 6, green)),
assertz(solve_cell(2, 7, green)),
assertz(solve_cell(2, 8, green)),
assertz(solve_cell(2, 9, blue)),

assertz(solve_cell(3, 1, blue)),
assertz(solve_cell(3, 2, blue)),
assertz(solve_cell(3, 3, blue)),
assertz(solve_cell(3, 4, blue)),
assertz(solve_cell(3, 5, blue)),
assertz(solve_cell(3, 6, blue)),
assertz(solve_cell(3, 7, blue)),
assertz(solve_cell(3, 8, blue)),
assertz(solve_cell(3, 9, blue)),

assertz(solve_cell(4, 2, blue)),
assertz(solve_cell(4, 4, blue)),
assertz(solve_cell(4, 6, green)),
assertz(solve_cell(4, 7, green)),
assertz(solve_cell(4, 8, green)),
assertz(solve_cell(4, 9, blue)),

assertz(solve_cell(5, 1, green)),
assertz(solve_cell(5, 2, blue)),
assertz(solve_cell(5, 3, green)),
assertz(solve_cell(5, 4, blue)),
assertz(solve_cell(5, 5, blue)),
assertz(solve_cell(5, 6, blue)),
assertz(solve_cell(5, 7, blue)),
assertz(solve_cell(5, 8, blue)),
assertz(solve_cell(5, 9, green)),

assertz(solve_cell(6, 1, blue)),
assertz(solve_cell(6, 2, green)),
assertz(solve_cell(6, 3, green)),
assertz(solve_cell(6, 4, blue)),
assertz(solve_cell(6, 6, blue)),
assertz(solve_cell(6, 8, blue)),
assertz(solve_cell(7, 1, blue)),
assertz(solve_cell(7, 2, blue)),
assertz(solve_cell(7, 3, blue)),
assertz(solve_cell(7, 4, blue)),
assertz(solve_cell(7, 5, blue)),
assertz(solve_cell(7, 6, green)),
assertz(solve_cell(7, 7, green)),
assertz(solve_cell(7, 8, blue)),
assertz(solve_cell(7, 9, blue)),

assertz(solve_cell(8, 1, blue)),
assertz(solve_cell(8, 2, green)),
assertz(solve_cell(8, 3, green)),
assertz(solve_cell(8, 4, green)),
assertz(solve_cell(8, 5, blue)),
assertz(solve_cell(8, 6, blue)),
assertz(solve_cell(8, 7, blue)),
assertz(solve_cell(8, 8, green)),
assertz(solve_cell(8, 9, green)),

assertz(solve_cell(9, 1, blue)),
assertz(solve_cell(9, 2, blue)),
assertz(solve_cell(9, 3, blue)),
assertz(solve_cell(9, 4, green)),
assertz(solve_cell(9, 5, green)),
assertz(solve_cell(9, 7, blue)),
assertz(solve_cell(9, 9, green)).