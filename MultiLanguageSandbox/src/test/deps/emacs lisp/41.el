(require 'cl-lib)

(defun calculate-expression (a b c)
  "Calculate the value of the expression (a + b) × c.
A, B, and C are integers.
Returns the result of (a + b) × c."
  (* (+ a b) c))


(defun check-calculate-expression ()
  (cl-assert (equal (calculate-expression 1 2 3) 9))
  (cl-assert (equal (calculate-expression -1 -2 2) -6))
  (cl-assert (equal (calculate-expression 0 0 1) 0))
  (cl-assert (equal (calculate-expression 100 -100 0) 0))
  (cl-assert (equal (calculate-expression 50 50 2) 200)))

(check-calculate-expression)