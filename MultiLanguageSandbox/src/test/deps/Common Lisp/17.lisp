(defun segments-intersect? (x1 y1 x2 y2 x3 y3 x4 y4)
  (let* ((denominator (- (* (- y4 y3) (- x2 x1)) (* (- x4 x3) (- y2 y1))))
         (ua-numerator (- (* (- x4 x3) (- y1 y3)) (* (- y4 y3) (- x1 x3))))
         (ub-numerator (- (* (- x2 x1) (- y1 y3)) (* (- y2 y1) (- x1 x3))))
         (ua (/ ua-numerator denominator))
         (ub (/ ub-numerator denominator)))
    (if (and (<= 0 ua 1) (<= 0 ub 1))
        (values (+ x1 (* ua (- x2 x1))) (+ y1 (* ua (- y2 y1))))
        nil)))


(defun check-segments-intersect? ()
(multiple-value-bind (x y) (segments-intersect? 0 0 1 1 1 0 0 1)
(assert (and (float-near-equal x 0.5) (float-near-equal y 0.5))))
(assert (null (segments-intersect? 0 0 1 1 2 2 3 3))))

(check-segments-intersect?)