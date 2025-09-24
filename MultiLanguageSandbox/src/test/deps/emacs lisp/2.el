(require 'cl-lib)

(defun longest-harmonious-subsequence (nums)
  "Find the length of the longest harmonious subsequence in NUMS.
A harmonious subsequence is one where the difference between max and min elements is exactly 1."
  (let ((freq (make-hash-table :test 'equal))
        (max-len 0))
    ;; Count frequency of each number
    (dolist (num nums)
      (puthash num (1+ (gethash num freq 0)) freq))
    
    ;; Check for pairs with difference 1
    (maphash (lambda (key val)
               (when (gethash (1+ key) freq)
                 (setq max-len (max max-len (+ val (gethash (1+ key) freq))))))
             freq)
    
    max-len))


(defun check-longest-harmonious-subsequence ()
(cl-assert (equal (longest-harmonious-subsequence '(1 3 2 2 5 2 3 7)) 5))
(cl-assert (equal (longest-harmonious-subsequence '(1 2 3 4)) 2))
(cl-assert (equal (longest-harmonious-subsequence '(1 1 1 1)) 0))
(cl-assert (equal (longest-harmonious-subsequence '()) 0))
(cl-assert (equal (longest-harmonious-subsequence '(1)) 0)))

(check-longest-harmonious-subsequence)