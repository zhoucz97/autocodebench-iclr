(defun calculate-year-end-amount (monthly-budgets)
  (let ((savings 0)
        (month 1))
    (dolist (budget monthly-budgets)
      (if (>= savings budget)
          (progn
            (decf savings budget)
            (incf month))
          (return-from calculate-year-end-amount (- month))))
    ;; If we get here, all months were successful
    (* savings 1.2)))


(defun test-calculate-year-end-amount ()
(assert (equal (calculate-year-end-amount '(290 230 280 200 300 170 340 50 90 80 200 60)) -7))
(assert (equal (calculate-year-end-amount '(100 100 100 100 100 100 100 100 100 100 100 100)) 2880.0)) ;; Assuming all savings
(assert (equal (calculate-year-end-amount '(300 300 300 300 300 300 300 300 300 300 300 300)) 0.0)) ;; No savings
(assert (equal (calculate-year-end-amount '(150 150 150 150 150 150 150 150 150 150 150 150)) 2160.0)) ;; Half savings
(assert (equal (calculate-year-end-amount '(250 250 250 250 250 250 250 250 250 250 250 250)) 720.0))) ;; Runs out first month
(test-calculate-year-end-amount)