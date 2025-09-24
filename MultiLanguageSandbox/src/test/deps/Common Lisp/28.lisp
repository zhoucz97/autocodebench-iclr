(defun total-cigarettes-smoked (initial-cigarettes butt-to-cigarette-ratio)
  (let ((total initial-cigarettes)
        (butts initial-cigarettes))
    (loop while (>= butts butt-to-cigarette-ratio) do
      (let ((new-cigarettes (floor butts butt-to-cigarette-ratio)))
        (incf total new-cigarettes)
        (setf butts (+ (mod butts butt-to-cigarette-ratio) new-cigarettes))))
    total))


(defun test-total-cigarettes-smoked ()
(assert (equal (total-cigarettes-smoked 4 3) 5))
(assert (equal (total-cigarettes-smoked 10 4) 13))
(assert (equal (total-cigarettes-smoked 20 5) 24))
(assert (equal (total-cigarettes-smoked 15 3) 22))
(assert (equal (total-cigarettes-smoked 7 2) 13))
(assert (equal (total-cigarettes-smoked 5 5) 6))
(assert (equal (total-cigarettes-smoked 0 3) 0)))

(test-total-cigarettes-smoked)