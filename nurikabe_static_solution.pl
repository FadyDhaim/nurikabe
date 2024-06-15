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

solve_cell(1, 1, green).
solve_cell(1, 3, blue).
solve_cell(1, 5, blue).
solve_cell(1, 6, blue).
solve_cell(1, 7, blue).
solve_cell(1, 8, blue).
solve_cell(1, 9, blue).

solve_cell(2, 1, blue).
solve_cell(2, 2, green).
solve_cell(2, 3, blue).
solve_cell(2, 4, green).
solve_cell(2, 5, green).
solve_cell(2, 6, green).
solve_cell(2, 7, green).
solve_cell(2, 8, green).
solve_cell(2, 9, blue).

solve_cell(3, 1, blue).
solve_cell(3, 2, blue).
solve_cell(3, 3, blue).
solve_cell(3, 4, blue).
solve_cell(3, 5, blue).
solve_cell(3, 6, blue).
solve_cell(3, 7, blue).
solve_cell(3, 8, blue).
solve_cell(3, 9, blue).

solve_cell(4, 2, blue).
solve_cell(4, 4, blue).
solve_cell(4, 6, green).
solve_cell(4, 7, green).
solve_cell(4, 8, green).
solve_cell(4, 9, blue).

solve_cell(5, 1, green).
solve_cell(5, 2, blue).
solve_cell(5, 3, green).
solve_cell(5, 4, blue).
solve_cell(5, 5, blue).
solve_cell(5, 6, blue).
solve_cell(5, 7, blue).
solve_cell(5, 8, blue).
solve_cell(5, 9, green).

solve_cell(6, 1, blue).
solve_cell(6, 2, green).
solve_cell(6, 3, green).
solve_cell(6, 4, blue).
solve_cell(6, 6, blue).
solve_cell(6, 8, blue).
solve_cell(7, 1, blue).
solve_cell(7, 2, blue).
solve_cell(7, 3, blue).
solve_cell(7, 4, blue).
solve_cell(7, 5, blue).
solve_cell(7, 6, green).
solve_cell(7, 7, green).
solve_cell(7, 8, blue).
solve_cell(7, 9, blue).

solve_cell(8, 1, blue).
solve_cell(8, 2, green).
solve_cell(8, 3, green).
solve_cell(8, 4, green).
solve_cell(8, 5, blue).
solve_cell(8, 6, blue).
solve_cell(8, 7, blue).
solve_cell(8, 8, green).
solve_cell(8, 9, green).

solve_cell(9, 1, blue).
solve_cell(9, 2, blue).
solve_cell(9, 3, blue).
solve_cell(9, 4, green).
solve_cell(9, 5, green).
solve_cell(9, 7, blue).
solve_cell(9, 9, green).

is_green_cell(Row, Column) :- fxd_cell(Row, Column, _); solve_cell(Row, Column, green).
is_blue_cell(Row, Column) :- solve_cell(Row, Column, blue).
is_cell_of_color(Row, Column, Color) :- (Color == green -> is_green_cell(Row, Column); Color == blue -> is_blue_cell(Row, Column)).
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
adjacent_cells_to_cell_of_color(Row, Column, Color, AdjacentCells) :-
    grid_size(Size),
    findall(
        [R, C],
        (
            (R is Row - 1, C is Column, R > 0, is_cell_of_color(R, C, Color));
            (R is Row + 1, C is Column, R =< Size, is_cell_of_color(R, C, Color));
            (R is Row, C is Column - 1, C > 0, is_cell_of_color(R, C, Color)); 
            (R is Row, C is Column + 1, C =< Size, is_cell_of_color(R, C, Color))
        ),
        AdjacentCells).



% Find all connected cells starting from (Row, Column)
connected_cells(Row, Column, ConnectedCells) :-
    connected_cells_helper(Row, Column, [], ConnectedCells).

% Helper predicate for finding connected cells
connected_cells_helper(Row, Column, Visited, ConnectedCells) :-
    \+ member([Row, Column], Visited),
    append(Visited, [[Row, Column]], NewVisited),
    adjacent_cells_to(Row, Column, AdjacentCells),
    findall(
        SubConnectedCells,
        (
            member([AdjRow, AdjCol], AdjacentCells),
            connected_cells_helper(AdjRow, AdjCol, NewVisited, SubConnectedCells)
        ),
        SubConnectedCellsList
    ),
    flatten([[[Row, Column]] | SubConnectedCellsList], ConnectedCells).

% Validation Rules
one_blue_region :-
    findall([Row, Column], is_blue_cell(Row, Column), BlueCells),
    (BlueCells = [] -> true;
    BlueCells = [[StartRow, StartColumn] | _],
    connected_cells(StartRow, StartColumn, ConnectedBlueCells),
    length(BlueCells, TotalBlueCells),
    length(ConnectedBlueCells, TotalBlueCells)).

no_2x2_blue_blocks :-
    \+ (between(1, 8, Row),
        between(1, 8, Column),
        is_blue_cell(Row, Column),
        NextRow is Row + 1,
        is_blue_cell(NextRow, Column),
        NextColumn is Column + 1,
        is_blue_cell(Row, NextColumn),
        is_blue_cell(NextRow, NextColumn)).

green_region_number_equals_size :-
    findall([Row, Column, Size], fxd_cell(Row, Column, Size), FixedCells),
    forall(member([Row, Column, Size], FixedCells),
           (connected_cells(Row, Column, ConnectedGreenCells),
            length(ConnectedGreenCells, Size))).

one_fixed_cell_in_green_region :-
    findall([Row, Column], fxd_cell(Row, Column, _), FixedCells),
    forall(member([Row, Column], FixedCells),
           (connected_cells(Row, Column, ConnectedGreenCells),
            findall([FR, FC], (member([FR, FC], ConnectedGreenCells), fxd_cell(FR, FC, _)), FixedCellsInGreen),
            length(FixedCellsInGreen, 1))).

% Validate the solution
validate :- one_blue_region, no_2x2_blue_blocks, green_region_number_equals_size, one_fixed_cell_in_green_region.

% Print the board and validate the solution
print_and_validate :- print_board(9), (validate -> writeln('Valid solution'); writeln('Invalid solution')).

% Start printing and validating
:- print_and_validate.