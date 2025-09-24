(require 'cl-lib)

(defun max-pens (yuan jiao)
  "Calculate the maximum number of pens that can be bought with YUAN and JIAO.
Each pen costs 1 yuan and 9 jiao."
  (let ((total-jiao (+ (* yuan 10) jiao))
        (pen-cost 19))  ; 1 yuan 9 jiao = 19 jiao
    (/ total-jiao pen-cost)))


(defun test-max-pens ()
(cl-assert (equal (max-pens 10 3) 5))
(cl-assert (equal (max-pens 5 0) 2))
(cl-assert (equal (max-pens 20 1) 10))
(cl-assert (equal (max-pens 0 19) 1))
(cl-assert (equal (max-pens 0 18) 0))
(cl-assert (equal (max-pens 1 8) 0)))

(test-max-pens)