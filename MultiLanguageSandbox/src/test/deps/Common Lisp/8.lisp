(defun list-to-dots (lst)
  (labels ((helper (x)
             (if (null x)
                 'nil
                 (list (car x) '. (helper (cdr x))))))
    (helper lst)))


(defun check-list-to-dots ()
;; 测试 'list-to-dots' 函数。
(assert (equal (list-to-dots '(a b c)) '(a . (b . (c . nil)))))
(assert (equal (list-to-dots '(1 2 3)) '(1 . (2 . (3 . nil)))))
(assert (equal (list-to-dots '(x y)) '(x . (y . nil))))
(assert (equal (list-to-dots '()) 'nil)))

(check-list-to-dots)