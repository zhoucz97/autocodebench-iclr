#lang racket
(require rackunit)

(define (minimum-queue-length distances)
  (apply + distances))


(define (check minimum-queue-length)
(define tests
(list (check-equal? (minimum-queue-length '(1 2 3)) 6)
(check-equal? (minimum-queue-length '(2 2 2)) 6)
(check-equal? (minimum-queue-length '(4 1 3 2)) 10)
(check-equal? (minimum-queue-length '(1 3 2 4)) 10)
(check-equal? (minimum-queue-length '(3 1 2)) 6)))
(andmap identity tests))

(check minimum-queue-length)