#lang racket
(require rackunit)

(define (reverse-three-digit num)
  (let ((hundreds (quotient num 100))
        (tens (quotient (remainder num 100) 10))
        (units (remainder num 10)))
    (+ (* units 100) (* tens 10) hundreds)))


(define (check reverse-three-digit)
  (check-equal? (reverse-three-digit 123) 321)
  (check-equal? (reverse-three-digit 400) 4) ; Note: Scheme automatically removes leading zeros in numbers.
  (check-equal? (reverse-three-digit 50) 5)  ; Adjusted example to fit Scheme's representation of numbers.
  (check-equal? (reverse-three-digit 678) 876)
  (check-equal? (reverse-three-digit 905) 509)
  (check-not-equal? (reverse-three-digit 123) 123))

(check reverse-three-digit)