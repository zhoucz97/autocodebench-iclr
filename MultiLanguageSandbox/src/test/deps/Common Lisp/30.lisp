(defun count-digit-two (L R)
  "Count the occurrences of the digit '2' in all integers within the range [L, R]."
  (labels ((count-twos-in-number (n)
             (let ((count 0))
               (loop while (> n 0)
                     do (let ((digit (mod n 10)))
                          (when (= digit 2)
                            (incf count))
                          (setf n (floor n 10))))
               count)))
    (let ((total 0))
      (loop for num from L to R
            do (incf total (count-twos-in-number num)))
      total)))


(defun test-count-digit-two ()
(assert (equal (count-digit-two 2 22) 6))
(assert (equal (count-digit-two 10 25) 8))
(assert (equal (count-digit-two 1 100) 20))
(assert (equal (count-digit-two 29 55) 4))
(assert (equal (count-digit-two 200 250) 66)))

(test-count-digit-two)