(defun conditional-square (x)
  (if (and (integerp x) (> x 0) (<= x 5))
      x
      (* x x)))


(defun check-conditional-square ()
;; 测试 'conditional-square' 函数。
(assert (equal (conditional-square 3) 3))
(assert (equal (conditional-square 5) 5))
(assert (equal (conditional-square 6) 36))
(assert (equal (conditional-square -3) 9))
(assert (equal (conditional-square 2.5) 6.25)))

(check-conditional-square)