;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.


(defun range (from to &optional step) 
  (let ((f from) 
	(res '()))
    (progn 
      (while (< f to) 
	(setq 
	 res (cons f res) 
	 f (+ f (if (null step) 1 step)))) 
      (reverse res))))

(defun set-union (first &rest rest) (delete-dups (append first (apply 'append rest))))

(defun set-diff ())

(defun select-row (vek i) (let* ((start (- i (% i 9))) 
				 (end (+ start 9))) 
			    (delete-dups (mapcar (lambda (i) (elt vek i)) (range start end)))))

(defun select-column (vek i) (let* ((start (% i 9)) 
				    (end (+ start 73))) 
			       (delete-dups (mapcar (lambda (i) (elt vek i)) (range start end 9)))))

(defun select-square (vek i) (let* ((x (- (% i 9)(% (% i 9) 3))) 
				    (y (- (/ i 9) (% (/ i 9) 3))) 
				    (s (+ x (* y 9))) 
				    (r (range s (+ s 3))) 
				    (rng (set-union r (mapcar (lambda (x) (+ x 9)) r) (mapcar (lambda (x) (+ x 18)) r)))) 
			       (delete-dups (mapcar (lambda (i) (elt vek i)) rng))))

(defun read-sudoku (vek buffer) 
  (if (< (length vek) 81) 
      (read-sudoku (cons (read buffer) vek) buffer) (coerce (reverse vek) 'vector)))

(defun solve-buffer (buffer) 
  (read-sudoku '() buffer))

(defun solve () (solve-buffer (get-buffer "puzzle4.txt")))



