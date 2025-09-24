#lang racket
(require rackunit)

(define (char-to-ascii char)
  (char->integer char))


(define (check char-to-ascii)
  (define tests
    (list (check-equal? (char-to-ascii #\A) 65)
          (check-equal? (char-to-ascii #\b) 98)
          (check-equal? (char-to-ascii #\1) 49)
          (check-equal? (char-to-ascii #\Z) 90)
          (check-equal? (char-to-ascii #\z) 122)
          (check-equal? (char-to-ascii #\!) 33)
          (check-equal? (char-to-ascii #\$) 36)))
  (andmap identity tests))

(check char-to-ascii)