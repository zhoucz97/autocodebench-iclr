#lang racket
(require rackunit)

(define (char-to-ascii char)
  (char->integer char))


(define (check char-to-ascii)
  (check-equal? (char-to-ascii #\A) 65)
  (check-equal? (char-to-ascii #\z) 122)
  (check-equal? (char-to-ascii #\0) 48)
  (check-equal? (char-to-ascii #\=) 61)
  (check-equal? (char-to-ascii #\!) 33)
  (check-not-equal? (char-to-ascii #\A) 66))

(check char-to-ascii)