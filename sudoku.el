;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.


(defun range (from to &optional step) 
  (let ((f from) (res '()))
    (progn 
      (while (< f to) 
	(setq 
	 res (cons f res) 
	 f (+ f (if (null step) 1 step)))) 
      (reverse res))))

(defun set-union ())
(defun set-diff ())


(defun select-row (vek index))
(defun select-column (vek index))
(defun select-square (vek index))

(defun read-sudoku (vek buffer) 
  (if (< (length vek) 81) 
      (read-sudoku (cons (read buffer) vek) buffer) (coerce (reverse vek) 'vector)))

(defun solve-buffer (buffer) (let   
	   ((vek (read-sudoku '() buffer)))
	   (length vek)))

(defun solve () (solve-buffer (get-buffer "board")))



