(require 'cl-lib)

(defun maximize-importance (budget items)
  "Calculate the maximum total importance within the BUDGET.
ITEMS is a list of cons cells (price . importance)."
  (let ((n (length items))
        (dp (make-vector (1+ budget) 0)))
    ;; Initialize the DP table
    (dotimes (i (1+ budget))
      (aset dp i 0))
    
    ;; Fill the DP table
    (dolist (item items)
      (let ((price (car item))
            (importance (cdr item)))
        (dotimes (i (1+ (- budget price)))
          (when (> (+ (aref dp i) (* price importance)) (aref dp (+ i price)))
            (aset dp (+ i price) (+ (aref dp i) (* price importance)))))))
    
    ;; The maximum importance is stored in the last element of the DP table
    (aref dp budget)))

;; Example usage:
(maximize-importance 1000 '((300 . 4) (200 . 3) (400 . 5) (100 . 2))) ; => 4000
(maximize-importance 500 '((150 . 3) (200 . 4) (100 . 2))) ; => 1450


(defun test-maximize-importance ()
(cl-assert (equal (maximize-importance 1000 '((300 . 4) (200 . 3) (400 . 5) (100 . 2))) 4000))
(cl-assert (equal (maximize-importance 500 '((150 . 3) (200 . 4) (100 . 2))) 1450))
(cl-assert (equal (maximize-importance 800 '((250 . 3) (350 . 4) (150 . 2))) 2450))
(cl-assert (equal (maximize-importance 600 '((100 . 1) (200 . 2) (300 . 3))) 1400)))

(test-maximize-importance)