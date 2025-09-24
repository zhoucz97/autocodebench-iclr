(defun power-of-two (n)
  "Calculates 2 raised to the power of n, where n is a non-negative integer (0 <= n < 31)."
  (expt 2 n))


(defun check-power-of-two ()
  (assert (= (power-of-two 3) 8))
  (assert (= (power-of-two 10) 1024))
  (assert (= (power-of-two 0) 1))
  (assert (= (power-of-two 5) 32))
  (assert (= (power-of-two 15) 32768)))

(check-power-of-two)