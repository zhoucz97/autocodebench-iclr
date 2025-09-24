(defun fourth-element (lst)
  (car (cdr (cdr (cdr lst)))))


(defun check-fourth-element ()
(assert (equal (fourth-element '(a b c d e f)) 'd))
(assert (equal (fourth-element '(1 2 3 4 5 6)) 4))
(assert (equal (fourth-element '("one" "two" "three" "four" "five")) "four")))

(check-fourth-element)