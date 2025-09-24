(require 'cl-lib)

(defun can-plant-flowers (flowerbed n)
  "Determine if N flowers can be planted in FLOWERBED without violating the non-adjacency rule.
FLOWERBED is a list of integers (0 or 1) where 0 represents an empty spot and 1 represents a planted flower.
N is the number of flowers to be planted.
Returns t if it is possible to plant N flowers, nil otherwise."
  (let ((count 0)
        (i 0)
        (len (length flowerbed)))
    (while (< i len)
      (if (and (zerop (nth i flowerbed))
               (or (zerop i) (zerop (nth (1- i) flowerbed)))
               (or (= i (1- len)) (zerop (nth (1+ i) flowerbed))))
          (progn
            (setq count (1+ count))
            (setf (nth i flowerbed) 1) ; Mark as planted
            (setq i (1+ i))) ; Skip next position
        (setq i (1+ i))))
    (>= count n)))


(defun check-can-plant-flowers ()
(cl-assert (equal (can-plant-flowers '(1 0 0 0 1) 1) t))
(cl-assert (equal (can-plant-flowers '(1 0 0 0 1) 2) nil))
(cl-assert (equal (can-plant-flowers '(0 0 0 0 0) 3) t))
(cl-assert (equal (can-plant-flowers '(0 0 1 0 1) 1) t))
(cl-assert (equal (can-plant-flowers '(1 0 1 0 1 0 1) 0) t))
(cl-assert (equal (can-plant-flowers '(0) 1) t)))

(check-can-plant-flowers)