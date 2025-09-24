(require 'cl-lib)

(defun find-error-nums (nums)
  "Find the duplicated and missing numbers in the list NUMS.
NUMS should be a list of integers from 1 to n with one duplicate and one missing.
Returns a list containing (duplicate missing)."
  (let ((n (length nums))
        (count (make-vector (1+ (length nums)) 0))
        duplicate missing)
    ;; Count occurrences of each number
    (dolist (num nums)
      (aset count num (1+ (aref count num))))
    
    ;; Find duplicate and missing numbers
    (dotimes (i (1+ n))
      (cond
       ((= (aref count i) 2) (setq duplicate i))
       ((= (aref count i) 0) (setq missing i))))
    
    (list duplicate missing)))


(defun check-find-error-nums ()
(cl-assert (equal (find-error-nums '(1 2 2 4)) '(2 3)))
(cl-assert (equal (find-error-nums '(1 3 3)) '(3 2)))
(cl-assert (equal (find-error-nums '(2 2)) '(2 1)))
(cl-assert (equal (find-error-nums '(1 5 3 2 2 4)) '(2 6)))
(cl-assert (equal (find-error-nums '(1 1)) '(1 2))))

(check-find-error-nums)