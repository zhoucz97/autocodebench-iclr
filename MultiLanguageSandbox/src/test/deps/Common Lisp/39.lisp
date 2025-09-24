(defun max-herb-value (time-limit herb-info)
  (let ((dp (make-array (1+ time-limit) :initial-element 0)))
    (dolist (herb herb-info)
      (let ((time (first herb))
            (value (second herb)))
        (loop for i from time-limit downto time do
          (setf (aref dp i) (max (aref dp i) (+ (aref dp (- i time)) value))))))
    (aref dp time-limit)))


(defun test-max-herb-value ()
(assert (equal (max-herb-value 70 '((71 100) (69 1) (1 2))) 3))
(assert (equal (max-herb-value 10 '((5 5) (4 7) (3 4) (2 3))) 14))
(assert (equal (max-herb-value 15 '((5 10) (10 15) (20 25))) 25))
(assert (equal (max-herb-value 100 '((50 60) (50 70))) 130))
(assert (equal (max-herb-value 5 '((2 3) (2 4) (1 1))) 8)))

(test-max-herb-value)