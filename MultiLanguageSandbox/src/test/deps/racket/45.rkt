#lang racket
(require rackunit)

(define (reverse-three-digit n)
  (+ (* (modulo n 10) 100)    ; last digit becomes first
     (* (modulo (quotient n 10) 10) 10)  ; middle digit becomes second
     (quotient n 100)))       ; first digit becomes last


(define (check reverse-three-digit)
  (define tests
    (list (check-equal? (reverse-three-digit 456) 654)
          (check-equal? (reverse-three-digit 700) 7)
          (check-equal? (reverse-three-digit 890) 98) ;; Adjusted for Racket's handling of numbers
          (check-equal? (reverse-three-digit 123) 321)
          (check-equal? (reverse-three-digit 250) 52)
          (check-equal? (reverse-three-digit 505) 505)
          (check-equal? (reverse-three-digit 999) 999)))
  (andmap identity tests))

(check reverse-three-digit)