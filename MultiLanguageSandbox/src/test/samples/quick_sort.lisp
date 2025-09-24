(defun quicksort (list)
  (if (or (null list) (null (cdr list)))
      list
      (let* ((pivot (car list))
             (rest (cdr list))
             (less (remove-if-not (lambda (x) (< x pivot)) rest))
             (greater (remove-if-not (lambda (x) (>= x pivot)) rest)))
        (append (quicksort less) (list pivot) (quicksort greater)))))


(defun check-quicksort ()
;; 测试 'quicksort' 函数。
(assert (equal (quicksort '(3 1 4 1 5 9 2 6 5 3 5)) '(1 1 2 3 3 4 5 5 5 6 9)))
(assert (equal (quicksort '(5 4 3 2 1)) '(1 2 3 4 5)))
(assert (equal (quicksort '()) '()))
(assert (equal (quicksort '(1)) '(1))))

(check-quicksort)
