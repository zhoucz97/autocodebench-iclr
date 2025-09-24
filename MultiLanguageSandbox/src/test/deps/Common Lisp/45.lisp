(defun int-bool-int-conversion (int)
  (if (zerop int) 0 1))


(defun check-int-bool-int-conversion ()
  (assert (= (int-bool-int-conversion 3) 1))
  (assert (= (int-bool-int-conversion 0) 0))
  (assert (= (int-bool-int-conversion -5) 1))
  (assert (= (int-bool-int-conversion 1) 1))
  (assert (= (int-bool-int-conversion 100) 1)))

(check-int-bool-int-conversion)