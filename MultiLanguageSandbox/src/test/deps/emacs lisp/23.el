(require 'cl-lib)

(defun goldbach-conjecture (N)
  "Test Goldbach's conjecture for even numbers from 4 to N.
Returns a list of lists where each sublist contains the even number and its prime pair."
  (let (result)
    (loop for even from 4 to N by 2
          do (let ((pair (find-goldbach-pair even)))
               (push (cons even pair) result)))
    (nreverse result)))


(defun test-goldbach-conjecture ()
(cl-assert (equal (goldbach-conjecture 10) '((4 2 2) (6 3 3) (8 3 5) (10 3 7))))
(cl-assert (equal (goldbach-conjecture 12) '((4 2 2) (6 3 3) (8 3 5) (10 3 7) (12 5 7))))
(cl-assert (equal (goldbach-conjecture 0) nil))
(cl-assert (equal (goldbach-conjecture 2) nil))
(cl-assert (equal (goldbach-conjecture 4) '((4 2 2)))))

(test-goldbach-conjecture)