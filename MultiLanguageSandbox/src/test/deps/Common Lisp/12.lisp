(defun sum-non-nil-elements (lst)
  (reduce #'+ (remove nil lst :key #'identity)))


(defun check-sum-non-nil-elements ()
(assert (equal (sum-non-nil-elements '(1 2 nil 4 5)) 12))
(assert (equal (sum-non-nil-elements '(nil nil nil)) 0))
(assert (equal (sum-non-nil-elements '(3 4 nil 6)) 13))
(assert (equal (sum-non-nil-elements '()) 0))
(assert (equal (sum-non-nil-elements '(nil 7 8)) 15)))

(check-sum-non-nil-elements)