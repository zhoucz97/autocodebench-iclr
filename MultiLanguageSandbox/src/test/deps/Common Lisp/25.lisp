(defun larger-prime-factor (n)
  "Finds the larger prime factor of a positive integer n, which is known to be the product of two distinct prime numbers."
  (let ((smaller (find-smallest-prime-factor n)))
    (/ n smaller)))

(defun find-smallest-prime-factor (n)
  "Finds the smallest prime factor of a positive integer n."
  (loop for i from 2 to (isqrt n)
        when (zerop (mod n i))
        return i
        finally (return n)))


(defun test-larger-prime-factor ()
(assert (equal (larger-prime-factor 21) 7))
(assert (equal (larger-prime-factor 15) 5))
(assert (equal (larger-prime-factor 33) 11))
(assert (equal (larger-prime-factor 35) 7))
(assert (equal (larger-prime-factor 77) 11))
(assert (equal (larger-prime-factor 26) 13)))

(test-larger-prime-factor)