#lang racket
(require rackunit)

(define (roman-to-int s)
  (let ((roman-values '((#\I . 1) (#\V . 5) (#\X . 10) (#\L . 50) 
                        (#\C . 100) (#\D . 500) (#\M . 1000))))
    (let loop ((chars (string->list s))
               (total 0)
               (prev-value 0))
      (if (null? chars)
          total
          (let* ((current-char (car chars))
                 (current-value (cdr (assoc current-char roman-values))))
            (if (< current-value prev-value)
                (loop (cdr chars) 
                      (- total prev-value (* 2 current-value)) 
                      current-value)
                (loop (cdr chars) 
                      (+ total current-value) 
                      current-value)))))))


;; Test cases
(define (check roman-to-int)
(check-equal? (roman-to-int "III") 3)
(check-equal? (roman-to-int "IV") 4)
(check-equal? (roman-to-int "IX") 9)
(check-equal? (roman-to-int "LVIII") 58)
(check-equal? (roman-to-int "MCMXCIV") 1994)
(check-equal? (roman-to-int "MCDXLIV") 1444)
)

(check roman-to-int)