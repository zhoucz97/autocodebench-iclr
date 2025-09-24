#lang racket
(require rackunit)

(define (power-of-two n)
  (if (= n 0)
      1
      (* 2 (power-of-two (- n 1)))))


(define (check power-of-two)
  (check-equal? (power-of-two 0) 1)
  (check-equal? (power-of-two 4) 16)
  (check-equal? (power-of-two 10) 1024)
  (check-equal? (power-of-two 5) 32)
  (check-equal? (power-of-two 15) 32768)
  (check-not-equal? (power-of-two 6) 32))

(check power-of-two)