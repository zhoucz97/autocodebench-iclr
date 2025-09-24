#lang racket
(require rackunit)

(define (ascii-to-char ascii-code)
  (integer->char ascii-code))


(define (check ascii-to-char)
  (define tests
    (list (check-equal? (ascii-to-char 65) #\A)
          (check-equal? (ascii-to-char 98) #\b)
          (check-equal? (ascii-to-char 49) #\1)
          (check-equal? (ascii-to-char 32) #\space)
          (check-equal? (ascii-to-char 90) #\Z)
          (check-equal? (ascii-to-char 122) #\z)
          (check-equal? (ascii-to-char 58) #\:)))
  (andmap identity tests))

(check ascii-to-char)