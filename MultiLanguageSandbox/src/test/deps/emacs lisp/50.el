(require 'cl-lib)

(defun absolute-value (n)
  "Calculate the absolute value of an integer N.
The absolute value of N should not exceed 10000.
Args:
  n: An integer whose absolute value is to be found.
Returns:
  The absolute value of N."
  (if (< n 0)
      (- n)
    n))


(defun check-absolute-value ()
  "Test cases for absolute-value function."
  (cl-assert (equal (absolute-value -5) 5))
  (cl-assert (equal (absolute-value 10) 10))
  (cl-assert (equal (absolute-value 0) 0))
  (cl-assert (equal (absolute-value -10000) 10000))
  (cl-assert (equal (absolute-value 9999) 9999)))

(check-absolute-value)