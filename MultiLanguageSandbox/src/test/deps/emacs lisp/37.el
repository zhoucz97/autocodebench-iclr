(require 'cl-lib)

(defun minimum-number-of-groups (price-limit prices)
  "Calculate the minimum number of groups needed to distribute souvenirs.
Each group can contain at most two souvenirs and the total price cannot exceed PRICE-LIMIT.
PRICES is a list of souvenir prices."
  (let ((sorted-prices (sort (copy-sequence prices) '>))
        (left 0)
        (right (1- (length prices)))
        (groups 0))
    (while (<= left right)
      (if (<= (+ (nth left sorted-prices) (nth right sorted-prices)) price-limit)
          (progn
            (setq left (1+ left))
            (setq right (1- right)))
        (setq right (1- right)))
      (setq groups (1+ groups)))
    groups))


(defun test-minimum-number-of-groups ()
(cl-assert (equal (minimum-number-of-groups 100 '(90 20 30 50 60 70 80 90)) 6))
(cl-assert (equal (minimum-number-of-groups 50 '(10 20 30 40 50)) 3))
(cl-assert (equal (minimum-number-of-groups 200 '(100 150 80 60)) 3))
(cl-assert (equal (minimum-number-of-groups 120 '(50 50 20 30)) 2)))

(test-minimum-number-of-groups)