#lang racket
(require rackunit)

(define (reverse-string s)
  (list->string (reverse (string->list s))))


(define (check reverse-string)
(check-equal? (reverse-string "hello") "olleh")
(check-equal? (reverse-string "world") "dlrow")
(check-equal? (reverse-string "") "")
(check-equal? (reverse-string "a") "a")
(check-equal? (reverse-string "racecar") "racecar")
(check-equal? (reverse-string "Scheme") "emehcS"))

(check reverse-string)