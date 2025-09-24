(defun count-valid-sequences (n)
  (let ((memo (make-hash-table :test 'eql)))
    (labels ((helper (k)
               (or (gethash k memo)
                   (setf (gethash k memo)
                         (if (<= k 1)
                             1
                             (+ 1 (loop for i from 1 to (floor k 2) sum (helper i))))))))
      (helper n))))


(defun test-count-valid-sequences ()
(assert (equal (count-valid-sequences 6) 6))
(assert (equal (count-valid-sequences 4) 4))
(assert (equal (count-valid-sequences 10) 14))
(assert (equal (count-valid-sequences 2) 2))
(assert (equal (count-valid-sequences 1) 1)))

(test-count-valid-sequences)