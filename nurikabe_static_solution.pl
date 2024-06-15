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
% solve_cell(1, 2, green).
solve_cell(1, 3, blue).
% solve_cell(1, 4, green).
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

% solve_cell(4, 1, green).
solve_cell(4, 2, blue).
% solve_cell(4, 3, green).
solve_cell(4, 4, blue).
% solve_cell(4, 5, green).
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
% solve_cell(6, 5, green).
solve_cell(6, 6, blue).
% solve_cell(6, 7, green).
solve_cell(6, 8, blue).
% solve_cell(6, 9, green).

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
% solve_cell(9, 6, green).
solve_cell(9, 7, blue).
% solve_cell(9, 8, green).
solve_cell(9, 9, green).

green_cell(Row, Column) :- fxd_cell(Row, Column, _); solve_cell(Row, Column, green).
blue_cell(Row, Column) :- solve_cell(Row, Column, blue).
% طباعة الرقعة
print_board(Size) :-
    nl,
    between(1, Size, Row),
    between(1, Size, Col),
    (fxd_cell(Row, Col, Num) ->
        write(Num)
    ; solve_cell(Row, Col, Color) ->
        (Color == green -> write('G') ; write('B'))
    ; write('_')),
    write(' '),
    (Col =:= Size -> nl ; true),
    fail.
print_board(_) :- nl.













% تحقق من صحة الحل
validate :- one_sea, no_2x2_blocks, island_number_equals_island_size, one_fixed_cell_in_island.




print_and_validate :- print_board(9), (validate ->
        writeln('Valid solution');
        writeln('Invalid solution')).

:- print_and_validate.