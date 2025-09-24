(defun count-output-sequences (n)
  (catalan-number n))


(defun test-count-output-sequences ()
(assert (equal (count-output-sequences 3) 5))
(assert (equal (count-output-sequences 4) 14))
(assert (equal (count-output-sequences 5) 42))
(assert (equal (count-output-sequences 6) 132))
(assert (equal (count-output-sequences 7) 429)))

(test-count-output-sequences)