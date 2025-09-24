(require 'cl-lib)

(defun reverse-digits (n)
  "Reverse the digits of integer N, removing leading zeros in the result.
Handles negative numbers by preserving the sign."
  (let* ((str (number-to-string n))
         (sign (if (string-prefix-p "-" str) "-" ""))
         (digits (if (string-prefix-p "-" str)
                     (substring str 1)
                   str))
         (reversed (concat (reverse digits))))
    (string-to-number (concat sign (replace-regexp-in-string "^0+" "" reversed)))))


(defun test-reverse-digits ()
(cl-assert (equal (reverse-digits 123) 321))
(cl-assert (equal (reverse-digits -123) -321))
(cl-assert (equal (reverse-digits 1200) 21))
(cl-assert (equal (reverse-digits 0) 0))
(cl-assert (equal (reverse-digits 5050) 505))
(cl-assert (equal (reverse-digits -5050) -505)))

(test-reverse-digits)