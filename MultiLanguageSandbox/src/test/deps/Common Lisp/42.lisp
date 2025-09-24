(defun char-to-ascii (char)
  "Converts a single visible character (excluding space) to its ASCII code."
  (check-type char character)
  (unless (char= char #\Space)
    (char-code char)))


(defun check-char-to-ascii ()
  (assert (= (char-to-ascii #\A) 65))
  (assert (= (char-to-ascii #\!) 33))
  (assert (= (char-to-ascii #\~) 126))
  (assert (= (char-to-ascii #\0) 48))
  (assert (= (char-to-ascii #\Z) 90)))

(check-char-to-ascii)