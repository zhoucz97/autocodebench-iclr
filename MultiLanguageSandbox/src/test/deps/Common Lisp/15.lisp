(defun nondecreasing? (numbers)
  (loop for (x y) on numbers
        while y
        always (<= x y)))


(defun check-nondecreasing? ()
(assert (equal (nondecreasing? '(1.0 2.0 2.0 3.0 4.0)) t))
(assert (equal (nondecreasing? '(1.0 2.0 3.0 3.9 4.0)) t))
(assert (equal (nondecreasing? '(1.0 2.0 1.0 4.0 5.0)) nil))
(assert (equal (nondecreasing? '(5.0 5.0 5.0 5.0)) t))
(assert (equal (nondecreasing? '(1.0)) t))
(assert (equal (nondecreasing? '()) t)))

(check-nondecreasing?)