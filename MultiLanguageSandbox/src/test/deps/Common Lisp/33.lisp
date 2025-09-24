(defun add-numbers (a b)
  (+ a b))


(defun test_add()
(assert (equal (add-numbers 1 2) 3))
(assert (equal (add-numbers 20 10) 30))
(assert (equal (add-numbers -1 -2) -3))
)
;; 运行测试
(test_add)