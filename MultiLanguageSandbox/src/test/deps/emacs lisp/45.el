(require 'cl-lib)

(defun reverse-three-digit-number (n)
  "Reverse a three-digit integer N.
Returns the reversed integer, with leading zeros not preserved."
  (let ((hundreds (floor n 100))
        (tens (floor (% n 100) 10))
        (units (% n 10)))
    (+ (* units 100) (* tens 10) hundreds)))


(defun check-reverse-three-digit-number ()
  (cl-assert (equal (reverse-three-digit-number 358) 853))
  (cl-assert (equal (reverse-three-digit-number 678) 876))
  (cl-assert (equal (reverse-three-digit-number 120) 21)))

(check-reverse-three-digit-number)