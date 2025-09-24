(require 'cl-lib)

(defun ascii-to-char (ascii-code)
  "Convert an ASCII CODE to the corresponding character.
ARGS:
  ascii-code: An integer representing an ASCII code.
RETURNS:
  The character corresponding to the ASCII code.
EXAMPLES:
  (ascii-to-char 65) => ?A
  (ascii-to-char 97) => ?a"
  (string (logand ascii-code 255)))


(defun check-ascii-to-char ()
  (cl-assert (equal (ascii-to-char 65) "A"))
  (cl-assert (equal (ascii-to-char 97) "a"))
  (cl-assert (equal (ascii-to-char 48) "0"))
  (cl-assert (equal (ascii-to-char 33) "!"))
  (cl-assert (equal (ascii-to-char 126) "~")))

(check-ascii-to-char)