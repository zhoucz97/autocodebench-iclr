#lang racket
(require rackunit)

(define (count-stairways n)
  (cond
    [(= n 1) 1]
    [(= n 2) 2]
    [else (+ (count-stairways (- n 1)) (count-stairways (- n 2)))]))


(define (check count-stairways)
(define tests
(list (check-equal? (count-stairways 0) 0)
(check-equal? (count-stairways 1) 1)
(check-equal? (count-stairways 2) 2)
(check-equal? (count-stairways 3) 3)
(check-equal? (count-stairways 4) 5)
(check-equal? (count-stairways 5) 8)
(check-equal? (count-stairways 6) 13)))
(andmap identity tests))

(check count-stairways)