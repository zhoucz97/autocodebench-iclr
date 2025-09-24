(require 'cl-lib)

(defun find-center-index (nums)
  "Find the center index of NUMS where the sum of elements to the left equals the sum of elements to the right.
If no such index exists, return -1."
  (let ((total-sum (apply '+ nums))
        (left-sum 0)
        (index 0))
    (while (< index (length nums))
      (let ((current (nth index nums)))
        (when (= left-sum (- total-sum left-sum current))
          (return index))
        (setq left-sum (+ left-sum current))
        (setq index (1+ index))))
    -1))


(defun check-find-center-index ()
(cl-assert (equal (find-center-index '(1 7 3 6 5 6)) 3))
(cl-assert (equal (find-center-index '(1 2 3)) -1))
(cl-assert (equal (find-center-index '(2 1 -1)) 0))
(cl-assert (equal (find-center-index '(-1 -1 -1 -1 -1 0)) 2))
(cl-assert (equal (find-center-index '(1)) 0)))

(check-find-center-index)