#lang racket
(require rackunit)

(define (int-bool-int-conversion x)
  (if (not (zero? x))
      1
      0))


(define (check int-bool-int-conversion)
  (define tests
    (list (check-equal? (int-bool-int-conversion 100) 1)
          (check-equal? (int-bool-int-conversion -1) 1)
          (check-equal? (int-bool-int-conversion 0) 0)
          (check-equal? (int-bool-int-conversion 1) 1)
          (check-equal? (int-bool-int-conversion -100) 1)
          (check-equal? (int-bool-int-conversion 9999) 1)
          (check-equal? (int-bool-int-conversion -9999) 1)))
  (andmap identity tests))

(check int-bool-int-conversion)