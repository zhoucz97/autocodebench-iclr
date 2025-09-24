(require 'cl-lib)

(defun dominant-index (nums)
  "Find the index of the dominant element in the list NUMS.
The dominant element is the largest element that is at least twice as large
as every other number in the list. Returns the index if found, -1 otherwise."
  (let ((max-val (apply 'max nums))
        (max-index (cl-position (apply 'max nums) nums))
        (dominant t))
    (dolist (num nums dominant)
      (when (and (/= num max-val)
                 (> (* 2 num) max-val))
        (setq dominant nil)))
    (if dominant max-index -1)))


;; Test cases
(defun check-dominant-index ()
(cl-assert (equal (dominant-index '(3 6 1 0)) 1))
(cl-assert (equal (dominant-index '(1 2 3 4)) -1))
(cl-assert (equal (dominant-index '(0 0 0 1)) 3))
(cl-assert (equal (dominant-index '(1)) 0))
(cl-assert (equal (dominant-index '(5 1 5 0 2)) 0)))

(check-dominant-index)