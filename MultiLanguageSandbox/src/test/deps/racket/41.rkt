#lang racket
(require rackunit)

(define (calculate-expression a b c)
  (* (+ a b) c))


(define (check calculate-expression)
  (define tests
    (list (check-equal? (calculate-expression 2 3 5) 25)
          (check-equal? (calculate-expression -2 3 -5) -5)
          (check-equal? (calculate-expression 0 100 0) 0)
          (check-equal? (calculate-expression 100 -50 2) 100)
          (check-equal? (calculate-expression -1 -1 1) -2)
          (check-equal? (calculate-expression 1000 2000 3) 9000)
          (check-equal? (calculate-expression -1000 500 2) -1000)))
  (andmap identity tests))

(check calculate-expression)