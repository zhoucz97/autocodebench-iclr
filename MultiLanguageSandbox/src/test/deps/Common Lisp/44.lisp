(defun reverse-three-digit (num)
  "Reverses a three-digit number, preserving leading zeros."
  (let* ((str (format nil "~3,'0d" num)) ; Convert num to a 3-digit string with leading zeros
         (reversed-str (reverse str)))   ; Reverse the string
    (parse-integer reversed-str)))       ; Convert the reversed string back to an integer

;; Example usage:
(reverse-three-digit 123) ; => 321
(reverse-three-digit 100) ; => 1


(defun check-reverse-three-digit ()
  (assert (= (reverse-three-digit 123) 321))
  (assert (= (reverse-three-digit 100) 1)) ;; Note: Leading zeros are not displayed in integers.
  (assert (= (reverse-three-digit 250) 52))
  (assert (= (reverse-three-digit 999) 999))
  (assert (= (reverse-three-digit 500) 5)))

(check-reverse-three-digit)