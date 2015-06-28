# emacs_sudoku
Sudoku game in emacs.

"num-key" to enter the number; "0" to clear the number

"r" to reset the board

"p" to get solution to current board (experimental) which'll disappear after moving cursor

"d" to save the current board

"l" to load a board

install:
1) download the "sudoku.el"
2) M-x load-file
3) enter the path of sudoku.el
start:
M-x sudoku

Tips:
1. If you change the code and want to test it, use C-x k to kill the sudoku buffer first if it's in buffer.
2. The solver will take so freaking long if the empty cell is many <- which is a bug. For example, if you enter "p" with a empty board, it'll stuck.
3. To the above problem, I'll try to use c to solve the problem (who'll give me power leaping through the sky :)