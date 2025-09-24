(require 'cl-lib)

(defun steps-to-swim-distance (distance)
  "Calculate the number of steps Xiaoyu needs to swim at least DISTANCE meters.
Each step is 98% of the previous step, starting with 2 meters."
  (let ((total 0.0)  ; Total distance swum so far
        (step 2.0)   ; Current step distance
        (count 0))   ; Step counter
    (while (< total distance)
      (setq total (+ total step))
      (setq step (* step 0.98))
      (setq count (1+ count)))
    count))


(defun test-steps-to-swim-distance ()
(cl-assert (equal (steps-to-swim-distance 4.3) 3))
(cl-assert (equal (steps-to-swim-distance 2) 1))
(cl-assert (equal (steps-to-swim-distance 5) 3))
(cl-assert (equal (steps-to-swim-distance 10) 6))
(cl-assert (equal (steps-to-swim-distance 15.5) 9)))

(test-steps-to-swim-distance)