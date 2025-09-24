(defun count-prime-sums (numbers k)
  "Count the number of ways to select k integers from numbers whose sum is prime."
  (let ((count 0))
    (dolist (combo (combinations numbers k) count)
      (when (is-prime (apply #'+ combo))
        (incf count)))))


(defun test-count-prime-sums ()
(assert (equal (count-prime-sums '(3 7 12 19) 3) 1))
(assert (equal (count-prime-sums '(1 2 3 4) 2) 4))
(assert (equal (count-prime-sums '(1 2 3 4 5 6) 3) 6))
(assert (equal (count-prime-sums '(10 20 30 40) 2) 0))
(assert (equal (count-prime-sums '(11 13 17 19 23 29) 3) 12))) 

(test-count-prime-sums)