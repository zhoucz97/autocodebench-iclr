#lang racket
(require rackunit)

(define (multiply a b)
  (* a b))


(define (check multiply)
  (check-equal? (multiply 5 6) 30)
  (check-equal? (multiply 10 10) 100)
  (check-equal? (multiply 500 100) 50000)
  (check-equal? (multiply 1 50000) 50000)
  (check-equal? (multiply 123 456) 56088)
  (check-not-equal? (multiply 2 2) 5))

(check multiply)