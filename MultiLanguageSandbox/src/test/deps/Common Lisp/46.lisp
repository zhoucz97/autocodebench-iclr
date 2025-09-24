(defun nth-term-arithmetic-sequence (a1 a2 n)
  "Calculates the nth term of an arithmetic sequence given the first two terms.
   Parameters:
     a1 - first term of the sequence
     a2 - second term of the sequence
     n - term number to find
   Returns:
     The value of the nth term in the arithmetic sequence."
  (let ((d (- a2 a1)))  ; Calculate the common difference
    (+ a1 (* (1- n) d))))  ; Calculate the nth term using the formula: a_n = a1 + (n-1)*d


(defun check-nth-term-arithmetic-sequence ()
  (assert (= (nth-term-arithmetic-sequence 1 4 100) 298))
  (assert (= (nth-term-arithmetic-sequence 2 5 10) 29))
  (assert (= (nth-term-arithmetic-sequence 0 3 4) 9))
  (assert (= (nth-term-arithmetic-sequence -3 1 5) 13))
  (assert (= (nth-term-arithmetic-sequence 100 -100 50) -9700)))

(check-nth-term-arithmetic-sequence)