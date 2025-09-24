#lang racket
(require rackunit)

(define (sort-votes candidate-count votes)
  (sort votes <))


(define (check sort-votes)
(define tests
(list (check-equal? (sort-votes 3 '(3 1 2 3 2)) '(1 2 2 3 3))
(check-equal? (sort-votes 4 '(4 4 1 2 3 2 1)) '(1 1 2 2 3 4 4))
(check-equal? (sort-votes 5 '(5 3 1 4 2 5 3 2 4)) '(1 2 2 3 3 4 4 5 5))
(check-equal? (sort-votes 2 '(2 2 1 1 2)) '(1 1 2 2 2))
(check-equal? (sort-votes 1 '(1 1 1 1 1)) '(1 1 1 1 1))))
(andmap identity tests))

(check sort-votes)