#lang racket
(require rackunit)

(define (unique-and-sort lst)
  (sort (remove-duplicates lst) <))


(define (check unique-and-sort)
(check-equal? (unique-and-sort '(4 1 2 3 2 1)) '(1 2 3 4))
(check-equal? (unique-and-sort '(5 3 5 7 8 9)) '(3 5 7 8 9))
(check-equal? (unique-and-sort '(10 20 10 20 30)) '(10 20 30))
(check-equal? (unique-and-sort '(100 200 300 200 100)) '(100 200 300))
(check-equal? (unique-and-sort '(55 42 42 55 60)) '(42 55 60))
(check-equal? (unique-and-sort '(9 8 7 6 5 4 3 2 1 0)) '(0 1 2 3 4 5 6 7 8 9)))

(check unique-and-sort)