#lang racket
(require rackunit)

(define (max-pens a b)
  (let ((total-yuan (+ a (/ b 10.0))))
    (floor (/ total-yuan 1.9))))


(define (check max-pens)
  (check-equal? (max-pens 20 5) 10.0)
  (check-equal? (max-pens 3 8) 2.0)
  (check-equal? (max-pens 0 9) 0.0)
  (check-equal? (max-pens 11 0) 5.0)
  (check-equal? (max-pens 50 5) 26.0)
  (check-not-equal? (max-pens 10 3) 4.0))

(check max-pens)