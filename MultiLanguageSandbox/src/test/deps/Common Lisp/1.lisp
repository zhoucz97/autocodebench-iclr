(defun has-close-elements (numbers threshold)
  (loop for i from 0 below (length numbers)
        do (loop for j from (1+ i) below (length numbers)
                 when (< (abs (- (nth i numbers) (nth j numbers))) threshold)
                 return t)
        finally (return nil)))


(defun check-has-close-elements ()
(assert (equal (has-close-elements '(1.0 2.0 3.9 4.0 5.0 2.2) 0.3) t))
(assert (equal (has-close-elements '(1.0 2.0 3.9 4.0 5.0 2.2) 0.05) nil))
(assert (equal (has-close-elements '(1.0 2.0 5.9 4.0 5.0) 0.95) t))
(assert (equal (has-close-elements '(1.0 2.0 5.9 4.0 5.0) 0.8) nil))
(assert (equal (has-close-elements '(1.0 2.0 3.0 4.0 5.0 2.0) 0.1) t))
(assert (equal (has-close-elements '(1.1 2.2 3.1 4.1 5.1) 1.0) t))
(assert (equal (has-close-elements '(1.1 2.2 3.1 4.1 5.1) 0.5) nil)))

(check-has-close-elements)