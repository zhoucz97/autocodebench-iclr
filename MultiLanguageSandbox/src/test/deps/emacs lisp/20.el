(require 'cl-lib)

(defun lemonade-change (bills)
  "Determine if it's possible to provide change for each customer at a lemonade stand.
BILLS is a list of integers representing the bills paid by each customer.
Returns t if change can be provided to every customer, nil otherwise."
  (let ((five 0)  ; number of $5 bills available
        (ten 0))  ; number of $10 bills available
    (catch 'cannot-provide-change
      (dolist (bill bills t)
        (cond
         ((= bill 5)
          (setq five (1+ five)))
         ((= bill 10)
          (if (zerop five)
              (throw 'cannot-provide-change nil)
            (setq five (1- five))
            (setq ten (1+ ten))))
         ((= bill 20)
          (cond
           ((and (>= ten 1) (>= five 1))
            (setq ten (1- ten))
            (setq five (1- five)))
           ((>= five 3)
            (setq five (- five 3)))
           (t
            (throw 'cannot-provide-change nil)))))))))


(defun check-lemonade-change ()
(cl-assert (equal (lemonade-change '(5 5 5 10 20)) t))
(cl-assert (equal (lemonade-change '(5 5 10 10 20)) nil))
(cl-assert (equal (lemonade-change '(5 5 10)) t))
(cl-assert (equal (lemonade-change '(10 10)) nil))
(cl-assert (equal (lemonade-change '(5 5 10 10 20 20 20 20 5 5)) nil)))

(check-lemonade-change)