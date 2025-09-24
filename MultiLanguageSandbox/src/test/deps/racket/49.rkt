#lang racket
(require rackunit)

(define (sign-of-n n)
  (cond
    [(> n 0) "positive"]
    [(< n 0) "negative"]
    [else "zero"]))


(define (check sign-of-n)
  (define tests
    (list (check-equal? (sign-of-n -100) "negative")
          (check-equal? (sign-of-n 100) "positive")
          (check-equal? (sign-of-n 0) "zero")
          (check-equal? (sign-of-n -999999999) "negative")
          (check-equal? (sign-of-n 999999999) "positive")
          (check-equal? (sign-of-n -1) "negative")
          (check-equal? (sign-of-n 1) "positive")))
  (andmap identity tests))

(check sign-of-n)