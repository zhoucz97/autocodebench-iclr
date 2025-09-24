(require 'cl-lib)

(defun process-random-numbers (numbers)
  "Process a list of random numbers by removing duplicates and sorting them.
Returns a list where the first element is the count of unique numbers,
and the second element is the sorted list of these unique numbers."
  (let ((unique-numbers (delete-dups numbers)))
    (list (length unique-numbers) (sort unique-numbers '<))))


(defun test-process-random-numbers ()
(cl-assert (equal (process-random-numbers '(10 20 30 20 10)) '(3 (10 20 30))))
(cl-assert (equal (process-random-numbers '(5 4 3 2 1 1)) '(5 (1 2 3 4 5))))
(cl-assert (equal (process-random-numbers '(55 23 67 23 90 100 90)) '(5 (23 55 67 90 100))))
(cl-assert (equal (process-random-numbers '(1 2 3 4 5)) '(5 (1 2 3 4 5)))))

(test-process-random-numbers)