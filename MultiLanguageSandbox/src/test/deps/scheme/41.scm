#lang racket
(require rackunit)

(define (calculate-expression a b c)
  (* (+ a b) c))


(define (check calculate-expression)
  (check-equal? (calculate-expression 1 2 3) 9)
  (check-equal? (calculate-expression -1 -2 2) -6)
  (check-equal? (calculate-expression 0 0 5) 0)
  (check-equal? (calculate-expression 5 5 -2) -20)
  (check-equal? (calculate-expression 3 2 4) 20)
  (check-not-equal? (calculate-expression 1 1 1) 3))

(check calculate-expression)