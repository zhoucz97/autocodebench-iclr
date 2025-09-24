(require 'cl-lib)

(defun has-close-elements (numbers threshold)
  "Check if any two numbers in NUMBERS are closer than THRESHOLD.
Returns t if such a pair exists, nil otherwise."
  (let ((sorted (sort (copy-sequence numbers) '<))
        (found nil))
    (catch 'done
      (dotimes (i (1- (length sorted)))
        (when (< (abs (- (nth (1+ i) sorted) (nth i sorted))) threshold)
          (setq found t)
          (throw 'done nil))))
    found))


;; Test cases
(defun check-has-close-elements ()
(cl-assert (equal (has-close-elements '(1.0 2.0 3.9 4.0 5.0 2.2) 0.3) t))
(cl-assert (equal (has-close-elements '(1.0 2.0 3.9 4.0 5.0 2.2) 0.05) nil))
(cl-assert (equal (has-close-elements '(1.0 2.0 5.9 4.0 5.0) 0.95) t))
(cl-assert (equal (has-close-elements '(1.0 2.0 5.9 4.0 5.0) 0.8) nil))
(cl-assert (equal (has-close-elements '(1.0 2.0 3.0 4.0 5.0 2.0) 0.1) t))
(cl-assert (equal (has-close-elements '(1.1 2.2 3.1 4.1 5.1) 1.0) t))
(cl-assert (equal (has-close-elements '(1.1 2.2 3.1 4.1 5.1) 0.5) nil)))

(check-has-close-elements)