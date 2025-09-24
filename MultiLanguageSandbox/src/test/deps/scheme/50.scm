#lang racket
(require rackunit)

(define (absolute-value n)
  (if (< n 0)
      (- n)
      n))


(define (check absolute-value)
  (check-equal? (absolute-value -10) 10)
  (check-equal? (absolute-value 5) 5)
  (check-equal? (absolute-value 0) 0)
  (check-equal? (absolute-value -10000) 10000)
  (check-equal? (absolute-value 9999) 9999)
  (check-not-equal? (absolute-value -1) -1))

(check absolute-value)