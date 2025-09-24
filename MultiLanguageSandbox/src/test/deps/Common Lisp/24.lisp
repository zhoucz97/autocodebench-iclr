(defun count-remaining-trees (road-length regions)
  (let ((merged-regions (merge-regions regions)))
    (- road-length (sum-regions-lengths merged-regions))))

(defun merge-regions (regions)
  (if (null regions)
      nil
      (let* ((sorted-regions (sort (copy-list regions) #'< :key #'car))
             (merged (list (first sorted-regions))))
        (dolist (region (rest sorted-regions) merged)
          (let* ((last-merged (first merged))
                 (last-start (car last-merged))
                 (last-end (cadr last-merged))
                 (current-start (car region))
                 (current-end (cadr region)))
            (if (<= current-start (1+ last-end))
                (setf (first merged) (list last-start (max last-end current-end)))
                (push region merged)))))))

(defun sum-regions-lengths (regions)
  (if (null regions)
      0
      (+ (1+ (- (cadr (first regions)) (car (first regions))))
         (sum-regions-lengths (rest regions)))))


(defun test-count-remaining-trees ()
(assert (equal (count-remaining-trees 10 '((2 5) (7 9))) 4))
(assert (equal (count-remaining-trees 15 '((1 3) (5 10))) 7))
(assert (equal (count-remaining-trees 20 '((0 5) (10 15))) 9))
(assert (equal (count-remaining-trees 30 '((3 6) (8 10) (15 20))) 18))
(assert (equal (count-remaining-trees 50 '((0 10) (20 30) (40 50))) 18)))

(test-count-remaining-trees)