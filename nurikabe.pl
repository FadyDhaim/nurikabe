% fixed cells
% الخلايا المرقمة يلي هيي جزء من الجزيرة
% fxd_cell(row, column, number of cells in the island)
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


% solve
% رح نكفي حل فعلي بناء عالخلايا المرقمة لحتى نقدر نمشي بالجزء يلي بعدو و يلي هوي التحقق من صحة الحل
% solve_cell(row, column, land or sea)
solve_cell(1,1,blue).
solve_cell(1,3,green).
solve_cell(1,4,green).