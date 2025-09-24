(require 'cl-lib)

(defun count-max-after-ops (m n ops)
  "Calculate the count of the maximum integer in an m x n matrix after applying operations.
Each operation [ai bi] increments all elements in the submatrix from (0,0) to (ai-1, bi-1) by 1.
The maximum value is the number of operations, and its count is the product of the smallest ai and bi in ops."
  (if (null ops)
      (* m n)  ; if no operations, all elements are 0, so count is m*n
    (let ((min-a (apply 'min (mapcar (lambda (op) (car op)) ops)))
          (min-b (apply 'min (mapcar (lambda (op) (cadr op)) ops))))
      (* min-a min-b))))


(defun check-count-max-after-ops ()
(cl-assert (equal (count-max-after-ops 3 3 '((2 2) (3 3))) 4))
(cl-assert (equal (count-max-after-ops 3 3 '((2 2) (3 1))) 2))
(cl-assert (equal (count-max-after-ops 4 4 '((1 1) (2 2))) 1))
(cl-assert (equal (count-max-after-ops 5 5 '()) 25))
(cl-assert (equal (count-max-after-ops 3 3 '((3 3) (2 2))) 4)))

(check-count-max-after-ops)