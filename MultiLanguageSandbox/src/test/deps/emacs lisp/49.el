(require 'cl-lib)

(defun determine-sign (n)
  "Determine the sign of an integer N.
Returns 'positive' if N > 0, 'zero' if N = 0, and 'negative' if N < 0."
  (cond
   ((> n 0) "positive")
   ((= n 0) "zero")
   (t "negative")))


(defun check-determine-sign ()
  (cl-assert (equal (determine-sign 95) "positive"))
  (cl-assert (equal (determine-sign 0) "zero"))
  (cl-assert (equal (determine-sign -10) "negative"))
  (cl-assert (equal (determine-sign 1) "positive"))
  (cl-assert (equal (determine-sign -1) "negative")))

(check-determine-sign)