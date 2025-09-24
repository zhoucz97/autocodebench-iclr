(require 'cl-lib)

(defun flood-fill (image sr sc new-color)
  "Perform flood fill on IMAGE starting at (SR, SC) with NEW-COLOR.
IMAGE is a 2D list of integers representing the image.
SR is the starting row index.
SC is the starting column index.
NEW-COLOR is the new color to apply.
Returns the modified image."
  (let* ((rows (length image))
         (cols (if (> rows 0) (length (car image)) 0))
         (original-color (nth sc (nth sr image))))
    ;; If the original color is the same as the new color, return the image unchanged
    (when (not (= original-color new-color))
      ;; Perform depth-first search for flood fill
      (labels ((dfs (r c)
                 (when (and (>= r 0) (< r rows)
                            (>= c 0) (< c cols)
                            (= (nth c (nth r image)) original-color))
                   (setf (nth c (nth r image)) new-color)
                   (dfs (1- r) c)  ; up
                   (dfs (1+ r) c)  ; down
                   (dfs r (1- c))  ; left
                   (dfs r (1+ c))))) ; right
        (dfs sr sc)))
    image))


;; Test cases
(defun check-flood-fill ()
(cl-assert (equal (flood-fill '((1 1 1) (1 1 0) (1 0 1)) 1 1 2) '((2 2 2) (2 2 0) (2 0 1))))
(cl-assert (equal (flood-fill '((0 0 0) (0 0 0)) 0 0 2) '((2 2 2) (2 2 2))))
(cl-assert (equal (flood-fill '((0 1 2) (1 1 1) (0 0 0)) 1 1 3) '((0 3 2) (3 3 3) (0 0 0)))))

(check-flood-fill)