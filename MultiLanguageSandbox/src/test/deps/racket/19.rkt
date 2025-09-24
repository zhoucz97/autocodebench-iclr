#lang racket
(require rackunit)

(define (third-largest-number lst)
  (let* ([sorted (sort lst >)]
         [len (length sorted)])
    (if (>= len 3)
        (list-ref sorted 2)
        (car sorted))))


(define (check third-largest-number)
(define tests
(list (check-equal? (third-largest-number (list 2 3 1)) 1)
(check-equal? (third-largest-number (list 1 2)) 2)
(check-equal? (third-largest-number (list 5 2 4)) 2)
(check-equal? (third-largest-number (list 10 10 10)) 10)
(check-equal? (third-largest-number (list 7 6 5 4)) 5)))
(andmap identity tests))

(check third-largest-number)