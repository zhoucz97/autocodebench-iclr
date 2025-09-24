#lang racket
(require rackunit)

(define (convert-to-uppercase letter)
  (string (integer->char (+ (char->integer (string-ref letter 0)) (- (char->integer #\A) (char->integer #\a))))))


(define (check convert-to-uppercase)
(check-equal? (convert-to-uppercase "a") "A")
(check-equal? (convert-to-uppercase "m") "M")
(check-equal? (convert-to-uppercase "z") "Z")
(check-equal? (convert-to-uppercase "g") "G")
(check-equal? (convert-to-uppercase "x") "X"))

(check convert-to-uppercase)