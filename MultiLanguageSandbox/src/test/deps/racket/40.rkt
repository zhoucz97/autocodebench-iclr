#lang racket
(require rackunit)

(define (remove-duplicates-and-sort numbers)
  (sort (remove-duplicates numbers) <))


(define (test-remove-duplicates-and-sort)
(define tests
(list
(check-equal? (remove-duplicates-and-sort '(3 1 4 1 5 9 2 6 5 3 5)) '(1 2 3 4 5 6 9))
(check-equal? (remove-duplicates-and-sort '(10 20 20 30 40 50)) '(10 20 30 40 50))
(check-equal? (remove-duplicates-and-sort '(5 4 3 2 1)) '(1 2 3 4 5))
(check-equal? (remove-duplicates-and-sort '(7 7 7 7 7 7 7)) '(7))
(check-equal? (remove-duplicates-and-sort '()) '())))
(andmap identity tests))

(test-remove-duplicates-and-sort)