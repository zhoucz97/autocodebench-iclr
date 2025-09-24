(require 'cl-lib)

(defun find-length-of-longest-increasing-subseq (nums)
  "Find the length of the longest consecutive increasing subsequence in NUMS.
NUMS is a list of integers.
Returns the length of the longest increasing subsequence."
  (if (null nums)
      0
    (let ((max-len 1)
          (current-len 1))
      (dolist (num (cdr nums) max-len)
        (if (> num (car nums))
            (progn
              (setq current-len (1+ current-len))
              (when (> current-len max-len)
                (setq max-len current-len)))
          (setq current-len 1))
        (setq nums (cdr nums))))))


(defun check-find-length-of-longest-increasing-subseq ()
(cl-assert (equal (find-length-of-longest-increasing-subseq '(1 3 5 4 7)) 3))
(cl-assert (equal (find-length-of-longest-increasing-subseq '(2 2 2 2 2)) 1))
(cl-assert (equal (find-length-of-longest-increasing-subseq '(1 2 3 4 5)) 5))
(cl-assert (equal (find-length-of-longest-increasing-subseq '(5 4 3 2 1)) 1))
(cl-assert (equal (find-length-of-longest-increasing-subseq '()) 0)))

(check-find-length-of-longest-increasing-subseq)