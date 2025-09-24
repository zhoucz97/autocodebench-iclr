(defun lines-from-file (filename)
  "Read each line from the file and return them as a list of strings."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))


(defun check-lines-from-file ()
(let ((lines (lines-from-file "../data/Common Lisp/file.txt")))
(assert (equal (first lines) "First line of the file"))
(assert (equal (second lines) "Second line of the file"))))

(check-lines-from-file)