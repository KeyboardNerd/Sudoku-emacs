# emacs_sudoku
Sudoku game in emacs.

"num-key" to enter the number; "0" to clear the number

"r" to reset the board

"p" to get solution to current board 

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
2. Compile solver.c using g++ for platform other than Mac version which is given.
