(require 'cl-lib)

(defun binary-search (nums target)
  "Perform binary search on a sorted list NUMS to find TARGET.
Returns the index of TARGET if found, -1 otherwise."
  (let ((left 0)
        (right (1- (length nums))))
    (while (<= left right)
      (let* ((mid (/ (+ left right) 2))
             (mid-val (nth mid nums)))
        (cond
         ((= mid-val target) (return mid))
         ((< mid-val target) (setq left (1+ mid)))
         (t (setq right (1- mid))))))
    -1))


(defun check-binary-search ()
(cl-assert (equal (binary-search '(-1 0 3 5 9 12) 9) 4))
(cl-assert (equal (binary-search '(-1 0 3 5 9 12) 2) -1))
(cl-assert (equal (binary-search '(1 2 3 4 5 6) 4) 3))
(cl-assert (equal (binary-search '(1 2 3 4 5 6) 6) 5))
(cl-assert (equal (binary-search '(1) 1) 0))
(cl-assert (equal (binary-search '() 1) -1)))

(check-binary-search)