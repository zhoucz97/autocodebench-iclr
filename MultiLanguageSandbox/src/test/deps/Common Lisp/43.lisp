(defun ascii-to-char (code)
  (code-char code))


(defun check-ascii-to-char ()
  (assert (char= (ascii-to-char 65) #\A))
  (assert (char= (ascii-to-char 33) #\!))
  (assert (char= (ascii-to-char 126) #\~))
  (assert (char= (ascii-to-char 48) #\0))
  (assert (char= (ascii-to-char 90) #\Z)))

(check-ascii-to-char)