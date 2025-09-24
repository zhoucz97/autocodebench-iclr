(defun min-box-space (box-capacity item-volumes)
  (let ((dp (make-array (1+ box-capacity) :initial-element 0)))
    (dolist (volume item-volumes)
      (loop for j from box-capacity downto volume do
        (setf (aref dp j) (max (aref dp j) (+ (aref dp (- j volume)) volume)))))
    (- box-capacity (aref dp box-capacity))))


(defun test-min-box-space ()
(assert (equal (min-box-space 20 '(5 5 10 6)) 0))
(assert (equal (min-box-space 25 '(7 8 5 10)) 0))
(assert (equal (min-box-space 12 '(3 2 2 5)) 0))
(assert (equal (min-box-space 30 '(10 10 12)) 8))
(assert (equal (min-box-space 8 '(1 2 3 4)) 0))
(assert (equal (min-box-space 18 '(2 5 6 8)) 2))
(assert (equal (min-box-space 11 '(1 2 3)) 5)))

(test-min-box-space)