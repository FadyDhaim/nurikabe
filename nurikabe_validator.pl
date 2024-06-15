:- module(nurikabe_validator, [validate/0]).


% تحقق من صحة الحل
validate :- one_sea, no_2x2_blocks, island_number_equals_island_size, one_fixed_cell_in_island.
