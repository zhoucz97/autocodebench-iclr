(require 'cl-lib)

(defun count-and-sort-numbers (numbers)
  "Count occurrences of each number in NUMBERS and return sorted pairs.
Each pair contains a number and its count, sorted by the numbers in ascending order."
  (let ((counts (make-hash-table :test 'equal))
        result)
    ;; Count occurrences of each number
    (dolist (num numbers)
      (puthash num (1+ (gethash num counts 0)) counts))
    
    ;; Convert hash table to alist and sort by number
    (setq result (sort (hash-table-to-alist counts)
                       (lambda (a b) (< (car a) (car b)))))
    
    result))


(defun test-count-and-sort-numbers ()
(cl-assert (equal (count-and-sort-numbers '(2 4 2 4 5 100 2 100)) '((2 3) (4 2) (5 1) (100 2))))
(cl-assert (equal (count-and-sort-numbers '(1 2 3 3 3 2 1)) '((1 2) (2 2) (3 3))))
(cl-assert (equal (count-and-sort-numbers '(5 4 3 2 1)) '((1 1) (2 1) (3 1) (4 1) (5 1))))
)
(test-count-and-sort-numbers)