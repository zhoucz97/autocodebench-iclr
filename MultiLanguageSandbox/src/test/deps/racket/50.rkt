#lang racket
(require rackunit)

(define (absolute-value n)
  (min (abs n) 10000))


(define (check absolute-value)
  (define tests
    (list (check-equal? (absolute-value -100) 100)
          (check-equal? (absolute-value 100) 100)
          (check-equal? (absolute-value 0) 0)
          (check-equal? (absolute-value -9999) 9999)
          (check-equal? (absolute-value 9999) 9999)
          (check-equal? (absolute-value -1) 1)
          (check-equal? (absolute-value 1) 1)))
  (andmap identity tests))

(check absolute-value)