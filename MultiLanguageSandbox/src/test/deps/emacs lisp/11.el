(require 'cl-lib)

(defun is-last-character-one-bit (bits)
  "Determine if the last character in a binary array must be a one-bit character.
A one-bit character is represented by 0, while two-bit characters are represented by 1 followed by another bit.
ARGS:
bits - List of integers representing the binary array.
RETURNS:
t if the last character is a one-bit character, nil otherwise."
  (let ((i 0))
    (while (< i (1- (length bits)))
      (setq i (+ i (1+ (nth i bits)))))
    (= i (1- (length bits)))))


(defun check-is-last-character-one-bit ()
(cl-assert (equal (is-last-character-one-bit '(1 0 0)) t))
(cl-assert (equal (is-last-character-one-bit '(1 1 1 0)) nil))
(cl-assert (equal (is-last-character-one-bit '(0)) t))
(cl-assert (equal (is-last-character-one-bit '(1 0)) nil))
(cl-assert (equal (is-last-character-one-bit '(1 1 0)) t)))

(check-is-last-character-one-bit)