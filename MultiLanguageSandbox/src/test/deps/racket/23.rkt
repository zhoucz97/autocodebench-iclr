#lang racket
(require rackunit)

(define (calculate-R2 R1 S)
  (- (* 2 S) R1))


(define (check calculate-R2)
(define tests
(list (check-equal? (calculate-R2 10 12) 14)
(check-equal? (calculate-R2 -5 0) 5)
(check-equal? (calculate-R2 7 8) 9)
(check-equal? (calculate-R2 15 20) 25)
(check-equal? (calculate-R2 0 -3) -6)
(check-equal? (calculate-R2 -10 -8) -6)))
(andmap identity tests))

(check calculate-R2)