#lang racket
(require rackunit)

(define (nth-term a1 a2 n)
  (let ((d (- a2 a1)))
    (+ a1 (* (- n 1) d))))


(define (check nth-term)
  (define tests
    (list (check-equal? (nth-term 0 3 4) 9)
          (check-equal? (nth-term 5 7 10) 23)
          (check-equal? (nth-term 1 1 100) 1)
          (check-equal? (nth-term 10 13 5) 22)
          (check-equal? (nth-term 100 50 3) 0)))
  (andmap identity tests))

(check nth-term)