(require 'cl-lib)

(defun mars-finger-permutation (n m current-permutation)
  "Calculate the new arrangement of a Martian's fingers after adding M to the current permutation.
N is the number of fingers, M is the small integer to add, and CURRENT-PERMUTATION is the current finger arrangement."
  (let* ((all-permutations (generate-permutations n))
         (current-index (find-permutation-index current-permutation all-permutations))
         (new-index (mod (+ current-index m) (length all-permutations))))
    (nth new-index all-permutations)))

(defun generate-permutations (n)
  "Generate all permutations of numbers from 1 to N."
  (if (= n 1)
      '((1))
    (let ((perms (generate-permutations (1- n))))
      (apply #'append
             (mapcar (lambda (x)
                       (mapcar (lambda (i)
                                 (insert-at x i n))
                               (number-sequence 0 (1- n))))
                     perms)))))

(defun insert-at (lst pos elem)
  "Insert ELEM into LST at position POS."
  (append (butlast lst pos) (list elem) (nthcdr (- (length lst) pos) lst)))

(defun find-permutation-index (permutation permutations)
  "Find the index of PERMUTATION in PERMUTATIONS."
  (cl-position permutation permutations :test #'equal))


(defun test-mars-finger-permutation ()
(cl-assert (equal (mars-finger-permutation 5 3 '(1 2 3 4 5)) '(1 2 4 5 3)))
(cl-assert (equal (mars-finger-permutation 3 2 '(1 2 3)) '(2 1 3)))
(cl-assert (equal (mars-finger-permutation 4 5 '(1 2 3 4)) '(1 4 3 2)))
(cl-assert (equal (mars-finger-permutation 6 10 '(1 2 3 4 5 6)) '(1 2 4 6 3 5))))

(test-mars-finger-permutation)