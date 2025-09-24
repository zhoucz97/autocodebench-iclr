#lang racket
(require rackunit)

(define (remove-value-and-return-length nums val)
  (let ([filtered (filter (lambda (x) (not (equal? x val))) nums)])
    (values filtered (length filtered))))


(define (check remove-value-and-return-length)
(define tests
(list (check-equal? (remove-value-and-return-length (list 3 2 2 3) 3) 2)
(check-equal? (remove-value-and-return-length (list 0 1 2 2 3 0 4 2) 2) 5)
(check-equal? (remove-value-and-return-length (list 1 2 3 4 5) 6) 5)
(check-equal? (remove-value-and-return-length (list 1 1 1 1) 1) 0)
(check-equal? (remove-value-and-return-length (list 5 5 5 2 2) 5) 2)))
(andmap identity tests))

(check remove-value-and-return-length)