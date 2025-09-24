(require 'cl-lib)

(defun longest-consecutive-sequence (sequence)
  "Return the length of the longest consecutive number sequence in SEQUENCE.
A consecutive sequence is a series where each number is one more than the previous."
  (let ((sorted (sort (copy-sequence sequence) '<))
        (max-length 1)
        (current-length 1))
    (when (null sorted)
      (return-from longest-consecutive-sequence 0))
    (dolist (num (cdr sorted) max-length)
      (if (= num (1+ (car (last (butlast sorted (length sorted) (- (length sorted) current-length -1)))))))
          (progn
            (setq current-length (1+ current-length))
            (when (> current-length max-length)
              (setq max-length current-length)))
        (setq current-length 1)))))


(defun test-longest-consecutive-sequence ()
(cl-assert (equal (longest-consecutive-sequence '(1 5 6 2 3 4 5 6 8 9)) 5))
(cl-assert (equal (longest-consecutive-sequence '(10 5 1 2 3)) 3))
(cl-assert (equal (longest-consecutive-sequence '(7 7 7 7)) 1))
(cl-assert (equal (longest-consecutive-sequence '(1 2 3 4 5)) 5))
)

(test-longest-consecutive-sequence)