#lang racket
(require rackunit)

(define (sum-arithmetic-series a1 a2 n)
  (let ((d (- a2 a1)))
    (* (/ n 2) (+ (* 2 a1) (* (- n 1) d)))))


(define (check sum-arithmetic-series)
(define tests
(list (check-equal? (sum-arithmetic-series 1 2 3) 6)
(check-equal? (sum-arithmetic-series -5 -10 5) -75)
(check-equal? (sum-arithmetic-series 5 7 4) 32)
(check-equal? (sum-arithmetic-series 3 6 10) 165)
(check-equal? (sum-arithmetic-series 10 15 7) 175)))
(andmap identity tests))

(check sum-arithmetic-series)