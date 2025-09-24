#lang racket
(require rackunit)

(define (extract-even-numbers lst)
  (filter even? lst))


(define (check extract-even-numbers)
(define tests
(list (check-equal? (extract-even-numbers (list 1 2 3 4 5 6)) '(2 4 6))
(check-equal? (extract-even-numbers (list 1 3 5 7)) '())
(check-equal? (extract-even-numbers (list 2 4 6 8)) '(2 4 6 8))
(check-equal? (extract-even-numbers (list 1 1 1 2)) '(2))
(check-equal? (extract-even-numbers '()) '())))
(andmap identity tests))

(check extract-even-numbers)