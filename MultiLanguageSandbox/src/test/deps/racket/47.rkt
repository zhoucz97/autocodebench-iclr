#lang racket
(require rackunit)

(define (multiply-a-b a b)
  (* a b))


(define (check multiply-a-b)
  (define tests
    (list (check-equal? (multiply-a-b 5 6) 30)
          (check-equal? (multiply-a-b 100 200) 20000)
          (check-equal? (multiply-a-b 1 50000) 50000)
          (check-equal? (multiply-a-b 250 100) 25000)
          (check-equal? (multiply-a-b 123 456) 56088)
          (check-equal? (multiply-a-b 50000 50000) 2500000000)
          (check-equal? (multiply-a-b 49999 2) 99998)))
  (andmap identity tests))

(check multiply-a-b)