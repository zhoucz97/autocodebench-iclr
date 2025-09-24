(defun ordered-union (list1 list2)
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (labels ((add-if-not-seen (element)
               (unless (gethash element seen)
                 (setf (gethash element seen) t)
                 (push element result))))
      ;; Process list1 first to preserve its order
      (dolist (x list1)
        (add-if-not-seen x))
      ;; Then process list2
      (dolist (x list2)
        (add-if-not-seen x))
      ;; Return the result in the correct order (we built it in reverse)
      (nreverse result))))


(defun check-ordered-union ()
(assert (equal (ordered-union '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6)))
(assert (equal (ordered-union '(1 2 3) '(3 4 5)) '(1 2 3 4 5)))
(assert (equal (ordered-union '(a b c) '(b c d)) '(a b c d)))
(assert (equal (ordered-union '() '(1 2 3)) '(1 2 3)))
(assert (equal (ordered-union '(1 2 3) '()) '(1 2 3))))

(check-ordered-union)