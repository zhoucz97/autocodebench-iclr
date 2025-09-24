(defun contains-nil? (lst)
  (some #'null lst))


(defun check-contains-nil? ()
(assert (equal (contains-nil? '(1 2 3 nil 4 5)) t))
(assert (equal (contains-nil? '(1 2 3 4 5)) nil))
(assert (equal (contains-nil? '(nil 1 2 3)) t))
(assert (equal (contains-nil? '()) nil))
(assert (equal (contains-nil? '(nil)) t)))

(check-contains-nil?)