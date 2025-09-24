(require 'cl-lib)

(defun max-product-of-three (nums)
  "Find the maximum product of any three numbers in the list NUMS.
Args:
  nums: List of integers.
Returns:
  The maximum product of three numbers."
  (let* ((sorted (sort (copy-sequence nums) #'<))
         (n (length sorted))
         (product1 (* (nth (- n 1) sorted) 
                      (nth (- n 2) sorted) 
                      (nth (- n 3) sorted)))
         (product2 (* (nth 0) sorted 
                      (nth 1) sorted 
                      (nth (- n 1) sorted))))
    (max product1 product2)))


(defun check-max-product-of-three ()
(cl-assert (equal (max-product-of-three '(1 2 3 4)) 24))
(cl-assert (equal (max-product-of-three '(-1 -2 -3 4)) 24))
(cl-assert (equal (max-product-of-three '(-4 -3 -2 -1 60)) 720))
(cl-assert (equal (max-product-of-three '(1 1 1 1)) 1))
(cl-assert (equal (max-product-of-three '(-1 -2 -3)) -6))
(cl-assert (equal (max-product-of-three '(-1 0 2 3 4)) 24)))

(check-max-product-of-three)