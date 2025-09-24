#lang racket
(require rackunit)

(define (arithmetic-sequence-nth-term a1 a2 n)
  (let ((d (- a2 a1)))
    (+ a1 (* (- n 1) d))))


(define (check arithmetic-sequence-nth-term)
  (check-equal? (arithmetic-sequence-nth-term 1 3 4) 7)
  (check-equal? (arithmetic-sequence-nth-term 5 7 10) 23)
  (check-equal? (arithmetic-sequence-nth-term -2 4 6) 28)
  (check-equal? (arithmetic-sequence-nth-term 0 2 50) 98)
  (check-equal? (arithmetic-sequence-nth-term 100 -100 11) -1900)
  (check-not-equal? (arithmetic-sequence-nth-term 1 1 1) 2))

(check arithmetic-sequence-nth-term)