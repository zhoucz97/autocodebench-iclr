(require 'cl-lib)

(defun nth-term-arithmetic-sequence (a1 a2 n)
  "Calculate the nth term of an arithmetic sequence.
A1 is the first term, A2 is the second term, and N is the term number to find."
  (let ((d (- a2 a1)))  ; Calculate the common difference
    (+ a1 (* (1- n) d))))  ; Calculate the nth term using the formula: a_n = a1 + (n-1)*d


(defun check-nth-term-arithmetic-sequence ()
  (cl-assert (equal (nth-term-arithmetic-sequence 1 4 100) 298))
  (cl-assert (equal (nth-term-arithmetic-sequence 3 5 10) 21))
  (cl-assert (equal (nth-term-arithmetic-sequence 5 7 1) 5))
  (cl-assert (equal (nth-term-arithmetic-sequence -3 -5 3) -7))
  (cl-assert (equal (nth-term-arithmetic-sequence 10 20 5) 50)))

(check-nth-term-arithmetic-sequence)