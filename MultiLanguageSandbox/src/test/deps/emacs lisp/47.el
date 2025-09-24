(require 'cl-lib)

(defun multiply-a-b (a b)
  "Multiply two positive integers A and B.
Both A and B must be between 1 and 50000 inclusive.
Returns the product of A and B."
  (unless (and (integerp a) (>= a 1) (<= a 50000))
    (error "A must be an integer between 1 and 50000"))
  (unless (and (integerp b) (>= b 1) (<= b 50000))
    (error "B must be an integer between 1 and 50000"))
  (* a b))


(defun check-multiply-a-b ()
  (cl-assert (equal (multiply-a-b 3 4) 12))
  (cl-assert (equal (multiply-a-b 36 18) 648))
  (cl-assert (equal (multiply-a-b 1 50000) 50000))
  (cl-assert (equal (multiply-a-b 250 200) 50000))
  (cl-assert (equal (multiply-a-b 123 456) 56088)))

(check-multiply-a-b)