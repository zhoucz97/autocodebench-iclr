(require 'cl-lib)

(defun total-swim-distance (start-day days)
  "Calculate the total distance swum by a fish over DAYS starting on START-DAY.
START-DAY is 1 for Monday, 2 for Tuesday, ..., 7 for Sunday.
The fish swims 250 km per day except on weekends (Saturday and Sunday)."
  (let ((total 0)
        (current-day start-day))
    (dotimes (i days total)
      (unless (or (= current-day 6) (= current-day 7))  ; Skip weekends (Saturday=6, Sunday=7)
        (setq total (+ total 250)))
      (setq current-day (1+ current-day))
      (when (> current-day 7)  ; Wrap around to Monday after Sunday
        (setq current-day 1)))))


(defun test-total-swim-distance ()
(cl-assert (equal (total-swim-distance 3 10) 2000))
(cl-assert (equal (total-swim-distance 1 7) 1250))
(cl-assert (equal (total-swim-distance 6 2) 0))
(cl-assert (equal (total-swim-distance 7 3) 500))
(cl-assert (equal (total-swim-distance 4 14) 2500))
(cl-assert (equal (total-swim-distance 1 0) 0)))

(test-total-swim-distance)