(require 'cl-lib)

(defun print-ascii-code (char)
  "Return the ASCII code of a given character.
CHAR should be a single character (excluding space).
Returns the ASCII code as an integer."
  (if (characterp char)
      (string-to-char (char-to-string char))
    (error "Input must be a character")))


(defun check-print-ascii-code ()
  (cl-assert (equal (print-ascii-code ?A) 65))
  (cl-assert (equal (print-ascii-code ?z) 122))
  (cl-assert (equal (print-ascii-code ?0) 48))
  (cl-assert (equal (print-ascii-code ?!) 33))
  (cl-assert (equal (print-ascii-code ?~) 126)))

(check-print-ascii-code)