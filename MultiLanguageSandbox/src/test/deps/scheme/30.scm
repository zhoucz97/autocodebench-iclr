#lang racket
(require rackunit)

(define (sum-of-arithmetic-sequence a1 a2 n)
  (let ((d (- a2 a1)))  ; Calculate the common difference
    (* (/ n 2) (+ (* 2 a1) (* (- n 1) d)))))  ; Apply the arithmetic series sum formula


(define (check sum-of-arithmetic-sequence)
(check-equal? (sum-of-arithmetic-sequence 1 2 3) 6)
(check-equal? (sum-of-arithmetic-sequence -5 -10 5) -75)
(check-equal? (sum-of-arithmetic-sequence 3 5 2) 8)
(check-equal? (sum-of-arithmetic-sequence 0 0 10) 0)
(check-equal? (sum-of-arithmetic-sequence 1 1 10) 10))

(check sum-of-arithmetic-sequence)