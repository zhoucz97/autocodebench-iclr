(defun index-of-element (element lst)
  (let ((index 0))
    (dolist (item lst nil)
      (when (equal item element)
        (return index))
      (incf index))))


(defun check-index-of-element ()
;; 测试 'index-of-element' 函数。
(assert (equal (index-of-element 'a '(a b c d e)) 0))
(assert (equal (index-of-element 'c '(a b c d e)) 2))
(assert (equal (index-of-element 'e '(a b c d e)) 4))
(assert (equal (index-of-element 'f '(a b c d e)) nil))
(assert (equal (index-of-element 3 '(1 2 3 4 5)) 2)))

(check-index-of-element)