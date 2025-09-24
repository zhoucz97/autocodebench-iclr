#lang racket
(require rackunit)

(define (sign-of-n n)
  (cond
    ((> n 0) "positive")
    ((= n 0) "zero")
    (else "negative")))


(define (check sign-of-n)
  (check-equal? (sign-of-n 10) "positive")
  (check-equal? (sign-of-n 0) "zero")
  (check-equal? (sign-of-n -5) "negative")
  (check-equal? (sign-of-n 1000000000) "positive")
  (check-equal? (sign-of-n -1000000000) "negative")
  (check-not-equal? (sign-of-n 1) "negative"))

(check sign-of-n)