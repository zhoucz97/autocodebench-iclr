(require 'cl-lib)

(defun power-of-two (n)
  "Calculate 2 raised to the power of N.
N should be a non-negative integer between 0 and 30 inclusive.
Returns the result as an integer."
  (if (or (< n 0) (>= n 31))
      (error "n must be between 0 and 30")
    (ash 1 n)))


(defun check-power-of-two ()
  (cl-assert (equal (power-of-two 3) 8))
  (cl-assert (equal (power-of-two 5) 32))
  (cl-assert (equal (power-of-two 0) 1))
  (cl-assert (equal (power-of-two 10) 1024))
  (cl-assert (equal (power-of-two 15) 32768)))

(check-power-of-two)