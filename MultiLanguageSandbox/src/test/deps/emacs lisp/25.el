(require 'cl-lib)

(defun count-water-pockets (heights)
  "Count the number of low points in HEIGHTS where water can accumulate.
A water pocket is a low point surrounded by higher terrain on both sides."
  (let ((count 0)
        (len (length heights)))
    (when (> len 2) ; Need at least 3 elements to have a pocket
      (dotimes (i (- len 2))
        (let ((current (nth i heights))
              (left (nth (+ i 1) heights))
              (right (nth (+ i 2) heights)))
          (when (and (< current left) (< current right))
            (setq count (1+ count))))))
    count))


(defun test-count-water-pockets ()
(cl-assert (equal (count-water-pockets '(0 1 0 2 1 2 0 0 2 0)) 3))
(cl-assert (equal (count-water-pockets '(0 2 2 1 2 0)) 1))
(cl-assert (equal (count-water-pockets '(0 3 3 3 0)) 0))
(cl-assert (equal (count-water-pockets '(0 1 2 3 2 1 0)) 0))
(cl-assert (equal (count-water-pockets '(0 1 0 1 0)) 1))
)

(test-count-water-pockets)