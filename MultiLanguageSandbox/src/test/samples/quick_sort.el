(require 'cl-lib)

(defun quicksort (list)
  (if (or (null list) (<= (length list) 1))
      (or list '())
    (let* ((pivot (car list))
           (rest (cdr list))
           (less (cl-remove-if-not (lambda (x) (< x pivot)) rest))
           (greater (cl-remove-if-not (lambda (x) (>= x pivot)) rest)))
      (append (quicksort less) (list pivot) (quicksort greater)))))

(defun check-quicksort ()
  (cl-assert (equal (quicksort '(3 1 4 1 5 9 2 6 5 3 5)) '(1 1 2 3 3 4 5 5 5 6 9)))
  (cl-assert (equal (quicksort '(5 4 3 2 1)) '(1 2 3 4 5)))
  (cl-assert (equal (quicksort '()) '()))
  (cl-assert (equal (quicksort '(1)) '(1))))

(check-quicksort)
