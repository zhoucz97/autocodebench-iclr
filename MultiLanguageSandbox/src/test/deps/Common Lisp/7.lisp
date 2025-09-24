(defun occurrences (lst)
  (let ((counts (make-hash-table :test 'eql)))
    ;; Count occurrences of each element
    (dolist (element lst)
      (incf (gethash element counts 0)))
    ;; Convert hash table to alist and sort by count (descending)
    (sort (loop for key being the hash-keys of counts
                using (hash-value value)
                collect (cons key value))
          #'> :key #'cdr)))


(defun check-occurrences ()
;; 测试 'occurrences' 函数。
(assert (equal (occurrences '(a b a d a c d c a)) '((a . 4) (c . 2) (d . 2) (b . 1))))
(assert (equal (occurrences '(1 2 2 3 3 3)) '((3 . 3) (2 . 2) (1 . 1))))
(assert (equal (occurrences '(a b a)) '((a . 2) (b . 1)))))

(check-occurrences)