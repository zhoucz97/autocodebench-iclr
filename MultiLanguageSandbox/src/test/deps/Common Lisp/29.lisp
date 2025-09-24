(defun divisible-five-digit-numbers (k)
  (let ((result '()))
    (loop for num from 10000 to 30000 do
      (let* ((str (write-to-string num))
             (sub1 (parse-integer (subseq str 0 3)))
             (sub2 (parse-integer (subseq str 1 4)))
             (sub3 (parse-integer (subseq str 2 5))))
        (when (and (zerop (mod sub1 k))
                   (zerop (mod sub2 k))
                   (zerop (mod sub3 k)))
          (push num result))))
    (if result
        (nreverse result)
        'No)))


(defun test-divisible-five-digit-numbers ()
(assert (equal (divisible-five-digit-numbers 15) '(22555 25555 28555 30000)))
(assert (equal (divisible-five-digit-numbers 31) '(15589 18682))) ;; No such numbers exist
(assert (equal (divisible-five-digit-numbers 999) 'No))) ;; No such numbers exist for a large divisor

(test-divisible-five-digit-numbers)