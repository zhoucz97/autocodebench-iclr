#lang racket
(require rackunit)

(define (check-integer-preferences x)
  (let ((even? (even? x))
        (in-range? (and (>= x 5) (<= x 12))))
    (list
     (if (and even? in-range?) 1 0)  ; A's preference
     (if (or even? in-range?) 1 0)   ; B's preference
     (if (xor even? in-range?) 1 0)  ; C's preference
     (if (not (or even? in-range?)) 1 0))))  ; D's preference

; Helper function for XOR (exclusive OR)
(define (xor a b)
  (or (and a (not b)) (and (not a) b)))


(define (test-check-integer-preferences)
(check-equal? (check-integer-preferences 10) '(1 1 0 0))
(check-equal? (check-integer-preferences 3) '(0 0 0 1))
(check-equal? (check-integer-preferences 13) '(0 0 0 1))
(check-equal? (check-integer-preferences 8) '(1 1 0 0))
(check-equal? (check-integer-preferences 5) '(0 1 1 0))
(check-equal? (check-integer-preferences 2) '(0 1 1 0)))

(test-check-integer-preferences)