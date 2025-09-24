#lang racket
(require rackunit)

(define (calculate-quotient a b)
  (floor (/ a b)))


(define (check-calculate-quotient)
(check-equal? (calculate-quotient 15 3) 5)
(check-equal? (calculate-quotient 20 4) 5)
(check-equal? (calculate-quotient 5 2) 2)
(check-equal? (calculate-quotient 7 3) 2)
(check-equal? (calculate-quotient 100 10) 10)
(check-equal? (calculate-quotient 0 5) 0))

(check-calculate-quotient)