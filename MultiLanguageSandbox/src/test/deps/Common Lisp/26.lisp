(defun day-of-max-unhappiness (weekly-schedule)
  (let ((max-unhappiness -1)
        (result-day 0))
    (loop for day in weekly-schedule
          for day-number from 1 to 7 do
          (let ((unhappiness (+ (first day) (second day))))
            (when (> unhappiness max-unhappiness)
              (setf max-unhappiness unhappiness
                    result-day day-number))))
    (if (> max-unhappiness 0)
        result-day
        0)))


(defun test-day-of-max-unhappiness ()
(assert (equal (day-of-max-unhappiness '((5 3) (6 2) (7 2) (5 3) (5 4) (0 4) (0 6))) 3))
(assert (equal (day-of-max-unhappiness '((4 4) (4 4) (4 4) (4 4) (4 4) (4 4) (4 4))) 0))
(assert (equal (day-of-max-unhappiness '((3 3) (3 3) (3 3) (3 3) (3 3) (3 3) (3 3))) 0))
(assert (equal (day-of-max-unhappiness '((2 6) (3 5) (4 4) (5 3) (6 2) (7 1) (1 7))) 0))
(assert (equal (day-of-max-unhappiness '((5 0) (0 5) (6 3) (3 6) (2 7) (7 2) (4 4))) 3)))
(test-day-of-max-unhappiness)