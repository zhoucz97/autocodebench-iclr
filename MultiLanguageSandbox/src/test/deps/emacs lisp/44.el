(require 'cl-lib)

(defun int-bool-int-conversion (int-val)
  "Convert an integer to a boolean and then back to an integer.
Non-zero integers become 1 (true), zero becomes 0 (false)."
  (if (not (zerop int-val))
      1
    0))


(defun check-int-bool-int-conversion ()
  (cl-assert (equal (int-bool-int-conversion 3) 1))
  (cl-assert (equal (int-bool-int-conversion 0) 0))
  (cl-assert (equal (int-bool-int-conversion -1) 1))
  (cl-assert (equal (int-bool-int-conversion 100) 1))
  (cl-assert (equal (int-bool-int-conversion -100) 1)))

(check-int-bool-int-conversion)