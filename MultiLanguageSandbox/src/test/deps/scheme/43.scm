#lang racket
(require rackunit)

(define (ascii-to-char code)
  (if (and (>= code 1) (<= code 127))
      (integer->char code)
      (error "Invalid ASCII code: must be between 1 and 127")))


(define (check ascii-to-char)
  (check-equal? (ascii-to-char 65) #\A)
  (check-equal? (ascii-to-char 122) #\z)
  (check-equal? (ascii-to-char 48) #\0)
  (check-equal? (ascii-to-char 33) #\!)
  (check-equal? (ascii-to-char 57) #\9)
  (check-not-equal? (ascii-to-char 65) #\B))

(check ascii-to-char)