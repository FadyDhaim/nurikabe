% fixed cells
% الخلايا المرقمة يلي هيي جزء من الجزيرة
% fxd_cell(row, column, number of cells in the island)
fxd_cell(1, 6, 3).
fxd_cell(1, 8, 3).
fxd_cell(3, 7, 3).
fxd_cell(4, 2, 6).
fxd_cell(5, 3, 2).
fxd_cell(5, 7, 4).
fxd_cell(6, 8, 3).
fxd_cell(7, 3, 1).
fxd_cell(9, 2, 6).
fxd_cell(9, 4, 4).


% solve
% رح نكفي حل فعلي بناء عالخلايا المرقمة لحتى نقدر نمشي بالجزء يلي بعدو و يلي هوي التحقق من صحة الحل
% solve_cell(row, column, land or sea)
solve_cell(1,1,blue).
solve_cell(1,3,green).
solve_cell(1,4,green).