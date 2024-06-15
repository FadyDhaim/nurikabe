% خلايا مرقمة او ثابتة
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

% الخلايا يلي بتمثل الحل ممكن تكون خضرا ك جزيرة او زرقا ك بحرر
:- dynamic solve_cell/3.

% تهيئة اللعبة بأنو نحذف خلايا الحل يلي خزناها بشكل ديناميكي
initialize_game :-
    retractall(solve_cell(_, _, _)).

% طباعة الرقعة
print_board(Size) :-
    between(1, Size, Row),
    between(1, Size, Col),
    (fxd_cell(Row, Col, Num) ->
        write(Num)
    ; solve_cell(Row, Col, Color) ->
        (Color =:= green -> write('G') ; write('B'))
    ; write('●')),
    (Col =:= Size -> nl ; true),
    fail.
print_board(_) :- nl.



% Example of a partial solution
solve_partially :-
    assertz(solve_cell(1, 1, blue)),
    assertz(solve_cell(1, 3, green)),
    assertz(solve_cell(1, 4, green)),
    print_board(9),
    retract(solve_cell(1, 1, blue)),
    retract(solve_cell(1, 3, green)),
    retract(solve_cell(1, 4, green)),
    % Continue with other assertions and retractions as needed
    assertz(solve_cell(1, 1, blue)), % Example continuation
    assertz(solve_cell(1, 3, green)),
    assertz(solve_cell(1, 5, blue)),
    print_board(9).

% Example strategy for solving a part of the board
solve :-
    initialize_game,
    solve_partially,
    (one_sea ->
        writeln('Valid solution');
        writeln('Invalid solution')).

% متل امبورت ل ملف تاني 
:- use_module(nurikabe_validator).

% Start solving
:- solve.
