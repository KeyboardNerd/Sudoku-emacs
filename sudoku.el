;; sudoku game in emacs
;; this is a practice in eLisp
;; Comes with no guarantee

(define-derived-mode sudoku-mode fundamental-mode "sudoku-mode"
  (define-key sudoku-mode-map (kbd "<up>") 'sudoku-input-up)
  (define-key sudoku-mode-map (kbd "<down>") 'sudoku-input-down)
  (define-key sudoku-mode-map (kbd "<left>") 'sudoku-input-left)
  (define-key sudoku-mode-map (kbd "<right>") 'sudoku-input-right)
  (define-key sudoku-mode-map (kbd "1") 'sudoku-debug-put1)
  (define-key sudoku-mode-map (kbd "2") 'sudoku-debug-put2)
(define-key sudoku-mode-map (kbd "3") 'sudoku-debug-put3)
(define-key sudoku-mode-map (kbd "4") 'sudoku-debug-put4)
(define-key sudoku-mode-map (kbd "5") 'sudoku-debug-put5)
(define-key sudoku-mode-map (kbd "6") 'sudoku-debug-put6)
(define-key sudoku-mode-map (kbd "7") 'sudoku-debug-put7)
(define-key sudoku-mode-map (kbd "8") 'sudoku-debug-put8)
(define-key sudoku-mode-map (kbd "9") 'sudoku-debug-put9)
(define-key sudoku-mode-map (kbd "0") 'sudoku-debug-rm)
(define-key sudoku-mode-map (kbd "l") 'sudoku-load-board)
(define-key sudoku-mode-map (kbd "p") 'sudoku-find-sol)
(define-key sudoku-mode-map (kbd "d") 'sudoku-design-board)
(define-key sudoku-mode-map (kbd "r") 'sudoku-restart)
)

(defvar *current-col* 0
  "The current cursor col value on board"
)
(defvar *current-row* 0
  "The current cursor row value on board"
)
(defvar *sudoku-block-size* 3 "The block size")
(defvar *sudoku-row-size* 9
  "The board row size"
)
(defvar *sudoku-col-size* 9
  "The board col size"
)
(defvar *sudoku-lower-bound* 1
  "The minimal possible value of the game"
)
(defvar *sudoku-upper-bound* 10
  "The board size as well as the exclusive upper bound of the game"
)
(defvar *sudoku-board* nil
  "The sudoku board with number from 0 to upper bound"
  )
(defvar *sudoku-loaded-list* nil
  "The list loaded to be a board"
  )
(defvar *sudoku-check-row* nil
  "1 if the number is existing in this row, 0 otherwise"
)
(defvar *sudoku-check-col* nil
 "1 if the number is existing in this col, 0 otherwise"
)
(defvar *sudoku-check-block* nil
  "1 if the number is existing in this block, 0 otherwise"
)
(defconst *sudoku-empty* 0)
(defconst *sudoku-occupied* 1)
(defconst *sudoku-empty-char* 0)
;;;###autoload
(defun sudoku()
  (interactive)
  (setq max-specpdl-size 10000)
  (setq max-lisp-eval-depth 500000)
  (switch-to-buffer "sudoku")
  (sudoku-mode)
  (sudoku-init)
  (erase-buffer)
  (ui-refresh)
)
;; initialize functions
(defun sudoku-init ()
  (let ((inhibit-read-only t))
  (erase-buffer)
  (setq *sudoku-board* (make-vector (* *sudoku-col-size* *sudoku-row-size*) *sudoku-empty-char*))
  (setq *sudoku-check-row* (make-vector (* *sudoku-row-size* (+ *sudoku-upper-bound* 1)) *sudoku-empty*))
  (setq *sudoku-check-col* (make-vector (* *sudoku-col-size* (+ *sudoku-upper-bound* 1)) *sudoku-empty*))
  (setq *sudoku-check-block* (make-vector (* *sudoku-row-size* (+ *sudoku-upper-bound* 1)) *sudoku-empty*)))
  )
;; restart function
(defun sudoku-restart()
  (interactive)
  (sudoku-init)
  (ui-refresh)
  )
  
;; debug put number
(defun sudoku-debug-put1 ()(interactive) (dp 1))
(defun sudoku-debug-put2 ()(interactive) (dp 2))
(defun sudoku-debug-put3 ()(interactive) (dp 3))
(defun sudoku-debug-put4 ()(interactive) (dp 4))
(defun sudoku-debug-put5 ()(interactive) (dp 5))
(defun sudoku-debug-put6 ()(interactive) (dp 6))
(defun sudoku-debug-put7 ()(interactive) (dp 7))
(defun sudoku-debug-put8 ()(interactive) (dp 8))
(defun sudoku-debug-put9 ()(interactive) (dp 9))
(defun sudoku-debug-rm ()(interactive) (rm))

;; board control events
(defun dp (number) "deploy number"
  (if (sudoku-put *current-col* *current-row* number) (ui-message-dpsuccess number) (ui-message-dpfail number))
  (when (sudoku-validate-all) (ui-message-win))
  (ui-refresh)
)
(defun rm () "remove number"
  (sudoku-remove *current-col* *current-row*)
  (ui-refresh)
)
;; cursor control events
(defun sudoku-input-up ()
  (interactive)
  (sudoku-cursor-move 0 -1)
  (ui-refresh)
)
(defun sudoku-input-down ()
  (interactive)
  (sudoku-cursor-move 0 1)
  (ui-refresh)
)
(defun sudoku-input-left ()
  (interactive)
  (sudoku-cursor-move -1 0)
  (ui-refresh)
)
(defun sudoku-input-right ()
  (interactive)
  (sudoku-cursor-move 1 0)
  (ui-refresh)
)
;; UI helper functions
(defun sudoku-cursor-move (colx rowx)
  (when (and (in-range 0 *sudoku-col-size* (+ *current-col* colx) ) (in-range 0 *sudoku-row-size* (+ *current-row* rowx)))
    (setq *current-col* (+ *current-col* colx))
    (setq *current-row* (+ *current-row* rowx))
  )
)
;; main interface
(defun ui-main-frame ()
  (ui-board)
  (ui-cursor-position)
)
(defun ui-refresh ()
  (let ((inhibit-read-only t))
  (erase-buffer)
  (ui-main-frame)
)
)
;; Textviews
(defun ui-board () ;; print board and current cursor
  (let ((inhibit-read-only t) (current-place (get-index *current-col* *current-row*)) (current-value nil ))
  (insert "\n = = = = = = The Board = = = = = = =\n")
  (dotimes (i (* *sudoku-row-size* *sudoku-col-size*))
    (if (equal *sudoku-empty* (elt *sudoku-board* i)) (setq current-value ".") (setq current-value (int-to-string (elt *sudoku-board* i))))
    (if (equal i current-place) (insert (format "|%s|" current-value)) (insert (format " %s "  current-value)))
    (when (eq 0 (% (+ i 1) 3)) (insert " | "))
    (when (eq 0 (% (+ i 1) (* *sudoku-row-size* *sudoku-block-size*))) (insert "\n -  -  -  -  -  -  -  -  -  -  -  -"))
    (when (eq 0 (% (+ i 1) 9)) (insert "\n"))
  )
  )
)
(defun ui-cursor-position ()
  (let ((inhibit-read-only t))
    (insert (format "\n = = = = = (%2d , %2d) = = = = =\n" *current-row* *current-col* ))
  )
)
;; message minibuffer
(defun ui-message-dpfail (number)
  (message (format "You can't place (%d) on row (%d), col (%d)!" number *current-row* *current-col*))
)
(defun ui-message-dpsuccess (number)
  (message (format "Successfully place (%d) on row (%d) col (%d)!" number *current-row* *current-col*))
)
(defun ui-message-win ()
  (message "Congraduation! You win the game!")
)
;; board generate function

;; board solve function
(defun sudoku-find-sol ()
  (interactive)
  (find-sol 0 0)
  (message "SOLUTION!")
)
(defun find-sol (col row)
  (let ((inhibit-read-only t))
  (catch 'exit
    (while (< col *sudoku-col-size*)
      (while (< row *sudoku-row-size*)
	(when (eq *sudoku-empty* (sudoku-get-board col row))
	  (dotimes (i *sudoku-upper-bound*)
	    (when (sudoku-validate col row i)
	      (sudoku-put col row i)
	      (find-sol col row)
	      (sudoku-remove col row)
	      )) 
	  (throw 'exit "break"))
	(setq row (+ row 1)))
      (setq col (+ col 1)) (setq row 0))
    (when (eq col *sudoku-col-size*) (ui-refresh))
 "break")))

;; board loading function
(defun sudoku-load-board (filename)
  (interactive "sPlease Enter the sudoku board file name:")
  (readfile filename)
  (sudoku-load-list *sudoku-board-list*)
)
;; board load a list to the main sudoku board buffer
(defun sudoku-load-list (list)
  (let ((inhibit-read-only t))
  (if (null list) throw 'exit "list is null")
  (if (<= (length list) (* *sudoku-row-size* *sudoku-col-size*)) (progn (sudoku-init) (dotimes (row *sudoku-row-size*) (dotimes (col *sudoku-col-size*) (sudoku-put col row (elt list (get-index col row)))))) (message "fail to load!"))
  (message (format "sudoku board loaded! List Length: %d" (length list) ))
  (ui-refresh))
  )

;; design board
(defun sudoku-design-board (stringname)
  "use the current UI to design a new board"
  (interactive "sPlease enter a name for the board")
  (with-temp-buffer
    (insert (list-to-string *sudoku-board*))
    (write-region nil nil stringname nil) ; default setting for IO is overwrite NOTICE 
  )
)
(defun list-to-string (list)
  (let ((str ""))
    (dotimes (i (length list))
      (setq str (concat str (number-to-string (elt list i))))
      (setq str (concat str " ")))
    (setq str str)))
;; board loading extenal file
(defun readfile (stringname)
  (with-temp-buffer
    (insert-file-contents stringname nil 0 500)
    (setq *sudoku-board-list* (list-integer (split-string (buffer-string) " " t)))
  )
)
;; helper function convert a list of integer string to a list of corresponding integer
(defun list-integer (list)
  (mapcar (lambda (arg) (string-to-number arg)) list)
)



;; board and check table change wrappers
(defun sudoku-put (col row number)
  "Wrapper when put a number in col row"
  (when (sudoku-validate col row number) (progn (sudoku-remove col row) (sudoku-set col row number)))
)
(defun sudoku-remove (col row)
  "set the value at col row to initial value (0) and remove it from check table"
  (when (and (sudoku-check-position col row)) (progn (sudoku-del-check col row (sudoku-get-board col row)) (sudoku-del-board col row)))
)
(defun sudoku-set (col row number)
  "set the value at col row to number and add it to check table"
  (when (sudoku-validate col row number) (progn (sudoku-set-check col row number) (set-board col row number)))
)

;; check functions wrapper
(defun sudoku-validate (col row number) "true if number can be put at col row, false otherwise"
 (when (and (sudoku-check-position col row) (sudoku-check-number number)) (sudoku-check-board col row number))
)
(defun sudoku-validate-all () "true if all cells contain number no less than *sudoku-lower-bound*, false otherwise"
 (let ((result 't))
   (dotimes (i (* *sudoku-row-size* *sudoku-col-size*)) (setq result (and result (sudoku-check-number (elt *sudoku-board* i)))))
   (equal result t)
 )
)
;; check functions
(defun sudoku-check-position (col row) (and (in-range 0 *sudoku-col-size* col) (in-range 0 *sudoku-row-size* row)))
(defun sudoku-check-number (number)  (in-range *sudoku-lower-bound*  *sudoku-upper-bound* number))
(defun sudoku-check-board (col row number) (sudoku-get-check col row number))
;; board table wrappers
;; TODO add check
(defun sudoku-del-board (col row) (set-board col row *sudoku-empty-char*))
(defun sudoku-set-board (col row number) (set-board col row number))
(defun sudoku-get-board (col row) (elt *sudoku-board* (get-index col row)))
;; check table wrappers
(defun sudoku-del-check (col row number) (sudoku-del-row row number) (sudoku-del-col col number) (sudoku-del-block col row number))
(defun sudoku-set-check (col row number) (sudoku-set-row row number) (sudoku-set-col col number) (sudoku-set-block col row number))
(defun sudoku-get-check (col row number) (and (sudoku-empty-row row number) (sudoku-empty-col col number) (sudoku-empty-block col row number)))
;; add value
(defun sudoku-set-row (row number) (set-row row number *sudoku-occupied*))
(defun sudoku-set-col (col number) (set-col col number *sudoku-occupied*))
(defun sudoku-set-block (col row number) (set-block col row number *sudoku-occupied*))
;; delete value
(defun sudoku-del-row (row number) (set-row row number *sudoku-empty*))
(defun sudoku-del-col (col number) (set-col col number *sudoku-empty*))
(defun sudoku-del-block (col row number) (set-block col row number *sudoku-empty*))
;; get value
(defun sudoku-get-row (row number) "return the value in the row check table cell"
 (elt *sudoku-check-row* (get-index row number)))
(defun sudoku-get-col (col number) "return the value in the col check table cell"
 (elt *sudoku-check-col* (get-index col number)))
(defun sudoku-get-block (col row number) "return the vaue in the block check table cell"
 (elt *sudoku-check-block* (get-index-block-number col row number)))
;; existence check
(defun sudoku-empty-row (row number) "return true if the number DOESN'T exist in the row, false otherwise"
 (eq *sudoku-empty* (sudoku-get-row row number)))
(defun sudoku-empty-col (col number) "return true if the number DOESN'T exist in the col, false otherwise"
 (eq *sudoku-empty* (sudoku-get-col col number)))
(defun sudoku-empty-block (col row number) "return true if the number DOESN'T exist in the block, false otherwise"
 (eq *sudoku-empty* (sudoku-get-block col row number)))

;; DEBUG FUNCTIONS 
(defun sudoku-debug-validate ()
  (let ((result t) (inhibit-read-only t))
  (dotimes (col 11)
    (dotimes (row 11)
      (dotimes (number 11)
	(let ((col (- col 1)) (row (- row 1)) (number (- number 1)))
	  (if (sudoku-validate col row number)
	    (insert (format "PASS col: %d, row: %d, number: %d\n" col row number))
	    (insert (format "---FAIL--- col: %d, row: %d, number: %d\n" col row number))
	)
	)
      )))
))

;; The helper functions
;; set value
(defun set-board (col row number) "Set the existence of number to true in the block which (col, row) in"
 (aset *sudoku-board* (get-index col row) number))
(defun set-row (row number which) "Set the existence of number to true in the row which (row) in"
 (aset *sudoku-check-row* (get-index row number) which))
(defun set-col (col number which) "Set the existence of number to true in the col which (col) in"
 (aset *sudoku-check-col* (get-index col number) which))
(defun set-block (col row number which) "set 1 to the corresponding block[col][row][number]"
  (aset *sudoku-check-block* (get-index-block-number col row number) which))
;; depreciated
(defun is-occupied-block (col row number) "1 if it's occupied in block (col, row) of number, 0 otherwise"
  (elt *sudoku-check-block* (get-index-block-number col row number)))
;; index conversions:
(defun get-index-block-number (col row number) "get the index of block check table for number"
  (get-index-raw (get-index-block col row) number *sudoku-upper-bound*))
(defun get-index-block (col row) "Get 2D array's col and row with index in 1D array ESPECIALLY for BLOCK"
  (setq col (/ col *sudoku-block-size*))
  (setq row (/ row *sudoku-block-size*))
  (get-index-raw col row *sudoku-block-size*))
(defun get-index-raw (col row rowsize) "Represent 2D array's col and row with index in 1D array"
  (+ col (* row rowsize)))
(defun get-index (col row) "Represent 2D array's col and row with index in 1D array specified to this size"
  (get-index-raw col row *sudoku-row-size*))
;; validator
(defun in-range (lowerbound upperbound number) " return true if lowerbound <= number < upperbound, false otherwise"
  (and (< number upperbound) (>= number lowerbound)))
