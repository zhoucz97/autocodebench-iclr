#lang racket
(require rackunit)

(define (find-unique-element lst)
  (let loop ((result 0) (remaining lst))
    (if (null? remaining)
        result
        (loop (bitwise-xor result (car remaining)) (cdr remaining)))))


(define (check find-unique-element)
(check-equal? (find-unique-element '(2 2 1)) 1)
(check-equal? (find-unique-element '(4 1 2 1 2)) 4)
(check-equal? (find-unique-element '(1)) 1)
(check-equal? (find-unique-element '(3 3 7 2 2)) 7)
(check-equal? (find-unique-element '(0 1 0)) 1))

(check find-unique-element)