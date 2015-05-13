;; emacs_sudoku.el -- sudoku game in Emacs!

(defun sudoku()
  "Main function for sudoku"
  (interactive)
  (switch-to-buffer "sudoku")
  (sudoku-mode)
  (sudoku-init)
  )

(define-derived-mode sudoku-mode special-mode "sudoku"
  (define-key sudoku-mode-map (kbd "SPC") 'sudoku-user-put)
  )

(defun sudoku-init()
  "Start a new game of sudoku."
  (setq *sudoku-board* (make-vector (* *size-board* *size-board* ) ?\.) )
  (sudoku-print-board)
  (setq *current-number* *sudoku-default-char*)
  )
(defconst *sudoku-default-char* 0
  "The default character used in the board"
  )
(defvar *current-number* nil
  "The current number the user using"
  )
(defvar *sudoku-board* nil
  "The game board of this program"
  )
(defvar *sudoku-initboard* nil
  "Indicating which space in the board can be changed"
  )
(defvar *sudoku-boollist* nil
  "Indicating if the col or row or subblock already has the number"
  )
(defconst *size-board* 9
  "The board size"
  )
(defvar boollist-row nil
  "The row size of boollist"
  )
(defvar boollist-col nil
  "The col size of the bollist")
(defun boollist-init()
  "Initialize the bool list"
  (setq boollist-row (* *size-board* *size-board*))
  (setq boollist-col (* *size-board* (1+ *size-board*)))
  (setq  *sudoku-boollist*
	 (make-vector
	  (* 3
	     (*
	      (* *size-board* *size-board*)
	      (* *size-board*
		 (1+ *size-board*)
		 )
	      )
	     )
	  ) nil
	 )  
  )
(defun sudoku-is-complete()
  (setq x 2)
  (loop for i from 0 to (* *size-board* *size-board*) do
    (if (not (elt *sudoku-board* i)) (setq x 3) nil)
   )
  (eq x 3)
  )
(defun sudoku-user-put(n)
  "set the current space"
  (interactive "nPlease enter a number: ")
  (sudoku-process n)
)
(defun sudoku-process(n)
  (interactive)
  (debug)
  (let ((row (1- (line-number-at-pos))) (col (current-column)))
    ( (+ 2 row) (+ 1 col)
    (set-number *sudoku-board* row col n)
  (if (sudoku-check n row col) (sudoku-put n row col) nil))
  )
  (sudoku-print-board)
)
(defun sudoku-put(n row col)
  set-number(n row col)
  (aset  *sudoku-boollist* 
    (+ 0 
      (+ 
        (* row boollist-row) 
        (* n 
          (* boollist-col boollist-row) 
          ) 
        )
      )
    n
    )

    (aset  *sudoku-boollist* 
    (+ 1 
      (+ 
        (* col boollist-row) 
        (* n 
          (* boollist-col boollist-row) 
          ) 
        )
      )
    n
    )

    (aset  *sudoku-boollist* 
    (+ 2 
      (+ 
        (* 
          (+ 
            (* (/ row *size-board*)
            *size-board*
            )
            (/ col *size-board*)
          )
          boollist-row) 
        (* n 
          (* boollist-col 
             boollist-row) 
          ) 
        )
      )
    n
    )

)

(defun sudoku-remove(n row col)
  sudoku-set-number(*sudoku-default-char* row col)
  (aset  *sudoku-boollist* 
    (+ 0 
      (+ 
        (* row boollist-row) 
        (* n 
          (* boollist-col boollist-row) 
          ) 
        )
      )
    nil
    )

    (aset  *sudoku-boollist* 
    (+ 1 
      (+ 
        (* col boollist-row) 
        (* n 
          (* boollist-col boollist-row) 
          ) 
        )
      )
    nil
    )

    (aset  *sudoku-boollist* 
    (+ 2 
      (+ 
        (* 
          (+ 
            (* (/ row *size-board*)
            *size-board*
            )
            (/ col *size-board*)
          )
          boollist-row) 
        (* n 
          (* boollist-col 
             boollist-row) 
          ) 
        )
      )
    nil
    )
)
(defun sudoku-check(n row col)
    (setq a (elt  *sudoku-boollist* 
    (+ 0 
      (+ 
        (* row boollist-row) 
        (* n 
          (* boollist-col boollist-row) 
          ) 
        )
      )
    )
    )

    (setq b (elt  *sudoku-boollist* 
    (+ 1 
      (+ 
        (* col boollist-row) 
        (* n 
          (* boollist-col boollist-row) 
          ) 
        )
      )
    )
    )

    (setq c (elt  *sudoku-boollist* 
    (+ 2 
      (+ 
        (* 
          (+ 
            (* (/ row *size-board*)
            *size-board*
            )
            (/ col *size-board*)
          )
          boollist-row) 
        (* n 
          (* boollist-col 
             boollist-row) 
          ) 
        )
      )
    )
    )

    (and (/= a nil) (and (/= b nil) (/= c nil)))
  )

(defun sudoku-changeable(row col)
  (get-number *sudoku-initboard* row col)
  )
(defun sudoku-print-board()
  "Print the board on the screen"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *size-board*)
      (dotimes (col *size-board*)
	(insert (sudoku-get-number row col))
	)
      (insert "\n")
      )
    )
  )

(defun init-set-number(row col val)
  (set-number(*sudoku-initboard* row col val))
  )
(defun sudoku-get-number (row col)
  (get-number *sudoku-board* row col)
  )


(defun get-number (board row col)
  "get the number in the (row col)"
  (elt board (+ col (* row *size-board*)))
  )

(defun set-number (board row col val)
  "set the number in the (row col) with (val)"
  (aset board (+ col (* row *size-board*)) val)
)

