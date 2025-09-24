(defun represent-as-powers-of-two (n)
  (labels ((helper (m acc)
             (if (zerop m)
                 (reverse acc)
                 (let* ((b (integer-length m))
                        (power (expt 2 (- b 1)))
                        (new-m (- m power)))
                   (helper new-m (cons (format nil "2(~a)" (- b 1)) acc))))))
    (let ((terms (helper n '())))
      (if (null terms)
          ""
          (apply #'concatenate 'string (intercalate "+" terms))))))


(defun test-represent-as-powers-of-two ()
(assert (equal (represent-as-powers-of-two 10) "2+2(2()+2)"))
(assert (equal (represent-as-powers-of-two 26) "2+2(2()+2)+2(2(2))"))
(assert (equal (represent-as-powers-of-two 1) "2()"))
(assert (equal (represent-as-powers-of-two 31) "2()+2+2(2)+2(2()+2)+2(2(2))"))
(assert (equal (represent-as-powers-of-two 100) "2(2)+2(2()+2(2))+2(2+2(2))")))

(test-represent-as-powers-of-two)