(defun count-a-in-list (lst)
  (count 'a lst))


(defun check-count-a-in-list ()
(assert (equal (count-a-in-list '(a b c d a e f a)) 3))
(assert (equal (count-a-in-list '(1 2 3 4 5)) 0))
(assert (equal (count-a-in-list '(a a a a)) 4))
(assert (equal (count-a-in-list '(b c d)) 0)))

(check-count-a-in-list)