(defun calculate-expression (a b c)
  (* (+ a b) c))


(defun check-calculate-expression ()
  (assert (= (calculate-expression 1 2 3) 9))
  (assert (= (calculate-expression -1 2 3) 3))
  (assert (= (calculate-expression 0 0 0) 0))
  (assert (= (calculate-expression 100 -100 1) 0))
  (assert (= (calculate-expression -5 -5 -5) 50))
  (assert (= (calculate-expression 10 20 30) 900))
  (assert (= (calculate-expression 0 10 -2) -20)))