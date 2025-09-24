(require 'cl-lib)

(defun next-greatest-letter (letters target)
  "Find the smallest character in LETTERS greater than TARGET.
If no such character exists, return the first character in LETTERS.
LETTERS is a sorted list of characters.
TARGET is a single character."
  (let ((result (car letters)))  ; Default to first character if not found
    (dolist (letter letters result)
      (when (and (string< target letter) 
                 (string< letter result))
        (setq result letter)))))


(defun check-next-greatest-letter ()
(cl-assert (equal (next-greatest-letter '("c" "f" "j") "a") "c"))
(cl-assert (equal (next-greatest-letter '("c" "f" "j") "c") "f"))
(cl-assert (equal (next-greatest-letter '("c" "f" "j") "d") "f"))
(cl-assert (equal (next-greatest-letter '("c" "f" "j") "g") "j"))
(cl-assert (equal (next-greatest-letter '("c" "f" "j") "j") "c"))
(cl-assert (equal (next-greatest-letter '("c" "f" "j") "k") "c")))

(check-next-greatest-letter)