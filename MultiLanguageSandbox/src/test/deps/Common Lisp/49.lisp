(defun judge-sign (n)
  (cond
    ((> n 0) "positive")
    ((< n 0) "negative")
    (t "zero")))


(defun check-judge-sign ()
  (assert (string= (judge-sign 95) "positive"))
  (assert (string= (judge-sign 0) "zero"))
  (assert (string= (judge-sign -3) "negative"))
  (assert (string= (judge-sign 1) "positive"))
  (assert (string= (judge-sign -1000000) "negative")))

(check-judge-sign)