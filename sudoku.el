(eval-when-compile (require 'cl))

(setq max-lisp-eval-depth 1024)

(defun select-row (board index) (let* ((start (- index (% index 9))) 
				       (end (+ start 9))) 
				  (delete-dups (mapcar (lambda (i) (elt board i)) (number-sequence start (1- end))))))

(defun select-column (board index) (let* ((start (% index 9)) 
					  (end (+ start 73))) 
				     (delete-dups (mapcar (lambda (i) (elt board i)) (number-sequence start (1- end) 9)))))

(defun select-square (board index) (let* ((x (- (% index 9)(% (% index 9) 3))) 
					  (y (- (/ index 9) (% (/ index 9) 3))) 
					  (s (+ x (* y 9))) 
					  (r (number-sequence s (+ s 2))) 
					  (rng (union r (append  (mapcar (lambda (x) (+ x 9)) r) (mapcar (lambda (x) (+ x 18)) r))))) 
				     (delete-dups (mapcar (lambda (i) (elt board i)) rng))))

(defun possible-values (board index) 
  (set-difference (number-sequence 1 9) (append (select-row board index) (select-column board index) (select-square board index))))

(defun smallest-change-set (board) (let ((sets (mapcar (lambda (x) (vector x (possible-values board x))) 
						       (remove-if-not (lambda (i) (zerop (elt board i))) (number-sequence 0 (1- (length board)))))))
				     (if sets (reduce (lambda (a b) (if (< (length (elt a 1)) (length (elt b 1))) a b)) sets))))

(defun solve (board) (let ((change (smallest-change-set board))) 
		       (if change (let ((index (elt change 0))
					(numbers (elt change 1))
					(solution)) 
				    (dolist (num numbers solution) (if (not solution) (setq solution (solve (copy-and-set board index num))))))
			 board)))

(defun copy-and-set (vek index elem) (let ((copy (copy-seq vek))) (aset copy index elem) copy))

(defun read-sudoku (buffer &optional board) 
  (if (< (length board) 81) 
      (read-sudoku buffer (cons (read buffer) board)) (coerce (reverse board) 'vector)))

(defun vec-to-char (vek) (mapcar (lambda (x) (number-to-string x)) vek))

(defun format-simple (vek) (if vek (apply 'concat vek) "No solution"))

(defun format-nice (vek) 
  (if vek (apply 'concat (mapcar (lambda (i) 
				   (concat 
				    (elt vek (1- i)) 
				    (if (zerop (% i 9)) (if (zerop (% i 27)) "\n\n" "\n") (if (zerop (% i 3)) "\s\s" "\s")))) 
				 (number-sequence 1 81))) "No solution"))

(defun read-board-and-solve () (let* ((board (read-sudoku (current-buffer))) 
				      (sol (solve board))) 
				 (vec-to-char sol)))

(defun sudoku-current-to-minibuffer () (interactive)  
  (save-excursion 
    (let ((sol (format-simple (read-board-and-solve))))
      (print (concat "Solution: " sol)))))
 
(defun sudoku-current-to-buffer (buffer) (interactive "bBuffer: ")  
  (save-excursion 
    (let ((sol (format-nice (read-board-and-solve))))
      (set-buffer (get-buffer buffer)) 
      (insert sol))))

(defun sudoku-buffer-to-current (buffer) (interactive "*bBuffer: ")  
  (insert 
   (save-excursion 
     (set-buffer (get-buffer buffer)) 
     (goto-char 0) 
     (format-nice (read-board-and-solve)))))

(defun sudoku-buffer-to-minibuffer (buffer) (interactive "bBuffer: ")  
  (save-excursion 
    (set-buffer (get-buffer buffer)) 
    (goto-char 0) 
    (print (concat "Solution: " (format-simple (read-board-and-solve))))))
