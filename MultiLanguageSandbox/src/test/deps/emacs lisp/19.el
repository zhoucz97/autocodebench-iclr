(require 'cl-lib)

(defun flip-and-invert-image (image)
  "Flip and invert a binary matrix.
FLIP means reversing each row, INVERT means changing 0 to 1 and 1 to 0.
IMAGE is a 2D list of binary values (0s and 1s).
Returns the flipped and inverted image."
  (mapcar (lambda (row)
            (mapcar (lambda (pixel)
                      (- 1 pixel))  ; Invert the pixel (0 becomes 1, 1 becomes 0)
                    (reverse row))) ; Flip the row by reversing it
          image))


(defun check-flip-and-invert-image ()
(cl-assert (equal (flip-and-invert-image '((1 1 0) (1 0 1) (0 0 0)))
'((1 0 0) (0 1 0) (1 1 1))))
(cl-assert (equal (flip-and-invert-image '((1 1 0 0) (1 0 0 1) (0 1 1 1) (1 0 1 0)))
'((1 1 0 0) (0 1 1 0) (0 0 0 1) (1 0 1 0))))
(cl-assert (equal (flip-and-invert-image '((0) (1)))
'((1) (0))))
(cl-assert (equal (flip-and-invert-image '((1)))
'((0)))))

(check-flip-and-invert-image)