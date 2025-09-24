(require 'cl-lib)

(defun unique-morse-representations (words)
  "Count the number of unique Morse code translations of WORDS.
ARGS:
  words - List of strings representing words.
RETURNS:
  The number of unique Morse code translations."
  (let ((morse-codes '(".-" "-..." "-.-." "-.." "." "..-." "--." "...." ".." ".---" "-.-" ".-.." "--" "-." "---" ".--." "--.-" ".-." "...-" "-" "..-" "..."))
        (translations (make-hash-table :test 'equal)))
    (dolist (word words)
      (let ((translation ""))
        (dotimes (i (length word))
          (let ((char (aref word i)))
            (setq translation (concat translation (nth (- char ?a) morse-codes)))))
        (puthash translation t translations)))
    (hash-table-count translations)))


(defun check-unique-morse-representations ()
(cl-assert (equal (unique-morse-representations '("gin" "zen" "gig" "msg")) 2))
(cl-assert (equal (unique-morse-representations '("a" "b" "c" "d")) 4))
(cl-assert (equal (unique-morse-representations '("cab" "abc" "bca")) 3)))

(check-unique-morse-representations)