#lang racket
(require rackunit)

(define (int-bool-int-conversion num)
  (if (not (zero? num)) 1 0))


(define (check int-bool-int-conversion)
  (check-equal? (int-bool-int-conversion 5) 1)
  (check-equal? (int-bool-int-conversion 0) 0)
  (check-equal? (int-bool-int-conversion -3) 1)
  (check-equal? (int-bool-int-conversion 100) 1)
  (check-equal? (int-bool-int-conversion -1) 1)
  (check-not-equal? (int-bool-int-conversion 1) 0))

(check int-bool-int-conversion)