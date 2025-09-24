#lang racket
(require rackunit)

(define (determine-winner n triangles)
  (if (odd? (- n 2))
      "JMcat Win"
      "PZ Win"))


(define (check determine-winner)
(check-equal? (determine-winner 5 '((0 1 2) (2 3 4) (4 1 0))) "PZ Win")
(check-equal? (determine-winner 4 '((0 1 2) (2 3 0))) "JMcat Win")
(check-equal? (determine-winner 6 '((0 1 2) (2 3 4) (4 5 0) (0 3 2))) "JMcat Win")
(check-equal? (determine-winner 7 '((0 1 2) (2 3 4) (4 5 6) (6 0 4))) "PZ Win"))

(check determine-winner)
