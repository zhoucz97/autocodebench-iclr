(require 'cl-lib)

(defun verify-isbn (isbn)
  "Verify the correctness of an ISBN number.
An ISBN is structured as x-xxx-xxxxx-x where 'x' is a digit and '-' is a separator.
The last digit is a checksum calculated as the sum of the first 9 digits multiplied
by their position index (1 to 9), modulo 11. If the checksum is 10, it is represented
as 'X'. Returns 'Right' if valid, or the corrected ISBN if invalid."
  (let* ((cleaned (replace-regexp-in-string "-" "" isbn))
         (digits (mapcar (lambda (c) 
                           (if (eq c ?X) 10 
                             (string-to-number (char-to-string c))))
                         (string-to-list cleaned)))
         (valid-length (and (= (length cleaned) 10)
                            (string-match-p "^[0-9X]+$" cleaned)))
         (checksum (mod (apply '+ 
                              (cl-loop for i from 1 to 9
                                       for d in (butlast digits)
                                       collect (* i d)))
                        11))
         (expected-checksum (if (= checksum 10) ?X (number-to-string checksum)))
         (actual-checksum (car (last digits))))
    (cond
     ((not valid-length) (error "Invalid ISBN length or characters"))
     ((= actual-checksum expected-checksum) "Right")
     (t (concat (substring isbn 0 (- (length isbn) 2))
                (if (eq expected-checksum ?X) "X" (number-to-string expected-checksum)))))))


(defun test-verify-isbn ()
(cl-assert (equal (verify-isbn "0-670-82162-4") "Right"))
(princ (verify-isbn "0-321-14670-2"))
(cl-assert (equal (verify-isbn "0-321-14670-2") "0-321-14670-0"))
(cl-assert (equal (verify-isbn "1-234-56789-X") "Right"))
(cl-assert (equal (verify-isbn "1-234-56789-0") "1-234-56789-X"))
(cl-assert (equal (verify-isbn "0-201-53082-1") "Right"))
(cl-assert (equal (verify-isbn "0-201-53082-X") "0-201-53082-1")))

(test-verify-isbn)