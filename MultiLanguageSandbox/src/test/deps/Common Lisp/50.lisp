(defun abs-value (n)
  (if (< n 0)
      (- n)
      n))


(defun check-abs-value ()
  (assert (= (abs-value -3) 3))
  (assert (= (abs-value 5) 5))
  (assert (= (abs-value 0) 0))
  (assert (= (abs-value -10000) 10000))
  (assert (= (abs-value 9999) 9999)))

(check-abs-value)