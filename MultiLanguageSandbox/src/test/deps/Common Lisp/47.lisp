(defun multiply-a-b (a b)
  (* a b))


(defun check-multiply-a-b ()
  (assert (= (multiply-a-b 3 4) 12))
  (assert (= (multiply-a-b 36 18) 648))
  (assert (= (multiply-a-b 1 50000) 50000))
  (assert (= (multiply-a-b 50000 50000) 2500000000))
  (assert (= (multiply-a-b 123 456) 56088)))

(check-multiply-a-b)