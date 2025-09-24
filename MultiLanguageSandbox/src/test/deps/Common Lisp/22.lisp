(defun min-n-for-sum-greater-than-k (k)
  "Calculates the smallest integer n such that the harmonic series sum S_n > k."
  (let ((sum 0.0)  ; Using floating-point for precision
        (n 1))
    (loop while (<= sum k) do
      (incf sum (/ 1.0 n))
      (incf n))
    (1- n)))  ; Return the last n where sum was <= k, then incremented


(defun test-min-n-for-sum-greater-than-k ()
(assert (equal (min-n-for-sum-greater-than-k 1) 2))
(assert (equal (min-n-for-sum-greater-than-k 2) 4))
(assert (equal (min-n-for-sum-greater-than-k 3) 11))
(assert (equal (min-n-for-sum-greater-than-k 4) 31))
(assert (equal (min-n-for-sum-greater-than-k 5) 83))
(assert (equal (min-n-for-sum-greater-than-k 0) 1)))

(test-min-n-for-sum-greater-than-k)