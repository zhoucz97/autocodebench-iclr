#lang racket
(require rackunit)

(define (power-of-two n)
  (expt 2 n))


(define (check power-of-two)
  (define tests
    (list (check-equal? (power-of-two 5) 32)
          (check-equal? (power-of-two 0) 1)
          (check-equal? (power-of-two 15) 32768)
          (check-equal? (power-of-two 10) 1024)
          (check-equal? (power-of-two 20) 1048576)
          (check-equal? (power-of-two 8) 256)
          (check-equal? (power-of-two 1) 2)))
  (andmap identity tests))

(check power-of-two)