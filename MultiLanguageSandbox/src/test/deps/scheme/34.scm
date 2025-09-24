#lang racket
(require rackunit)

(define (compare-areas a b c)
  (let ((square-area (* a a))
        (rectangle-area (* b c)))
    (cond
      ((> square-area rectangle-area) "Alice")
      ((< square-area rectangle-area) "Bob")
      (else "Equal"))))


(define (test-compare-areas)
(check-equal? (compare-areas 3 2 4) "Alice")
(check-equal? (compare-areas 5 3 3) "Alice")
(check-equal? (compare-areas 4 2 4) "Alice")
(check-equal? (compare-areas 6 2 5) "Alice")
(check-equal? (compare-areas 2 3 4) "Bob")
(check-equal? (compare-areas 4 4 4) "Equal"))

(test-compare-areas)