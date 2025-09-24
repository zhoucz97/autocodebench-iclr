(require 'cl-lib)

(defun is-toeplitz-matrix (matrix)
  "Check if MATRIX is a Toeplitz matrix.
A Toeplitz matrix has constant values along each diagonal from top-left to bottom-right.
MATRIX is a 2D list of integers.
Returns t if the matrix is Toeplitz, nil otherwise."
  (let ((rows (length matrix))
        (cols (if (> rows 0) (length (car matrix)) 0)))
    ;; Handle empty matrix case
    (when (or (zerop rows) (zerop cols))
      (return-from is-toeplitz-matrix t))
    
    ;; Check all diagonals starting from first row
    (dotimes (j (- cols 1))
      (let ((val (nth j (car matrix))))
        (let ((i 1)
              (k (+ j 1)))
          (while (and (< i rows) (< k cols))
            (when (not (equal val (nth k (nth i matrix))))
              (return-from is-toeplitz-matrix nil))
            (setq i (1+ i))
            (setq k (1+ k))))))
    
    ;; Check all diagonals starting from first column (excluding first element)
    (dotimes (i (- rows 1))
      (let ((val (nth 0 (nth i matrix))))
        (let ((j 1)
              (k (1+ i)))
          (while (and (< j cols) (< k rows))
            (when (not (equal val (nth j (nth k matrix))))
              (return-from is-toeplitz-matrix nil))
            (setq j (1+ j))
            (setq k (1+ k))))))
    
    t))


(defun check-is-toeplitz-matrix ()
(cl-assert (equal (is-toeplitz-matrix '((1 2 3 4) (5 1 2 3) (9 5 1 2))) t))
(cl-assert (equal (is-toeplitz-matrix '((1 2) (2 2))) nil))
(cl-assert (equal (is-toeplitz-matrix '((1 2 3) (4 1 2) (5 4 1) (6 5 4))) t))
(cl-assert (equal (is-toeplitz-matrix '((1))) t))
(cl-assert (equal (is-toeplitz-matrix '((1 2) (1 2))) nil)))

(check-is-toeplitz-matrix)