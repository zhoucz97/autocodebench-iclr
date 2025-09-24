(defun contains-list? (lst)
  (cond ((null lst) nil)  ; base case: empty list
        ((listp (car lst)) t)  ; if first element is a list, return t
        (t (contains-list? (cdr lst)))))  ; otherwise check the rest of the list


(defun check-contains-list? ()
;; 测试 'contains-list?' 函数。
(assert (equal (contains-list? '(1 2 (3 4) 5)) t))
(assert (equal (contains-list? '(1 2 3 4 5)) nil))
(assert (equal (contains-list? '((1 2) 3 4 5)) t))
(assert (equal (contains-list? '()) nil))
(assert (equal (contains-list? '("string" 123 (a b))) t)))

(check-contains-list?)