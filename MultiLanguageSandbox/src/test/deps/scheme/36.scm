#lang racket
(require rackunit)

(define (remaining-apples m t s)
  (let ((total-time-needed (* m t)))
    (if (>= total-time-needed s)
        (- m (quotient s t))
        0)))


(define (check remaining-apples)
(check-equal? (remaining-apples 5 5 15) 2) ; 5 apples, 5 mins each, 15 mins passed, 2 remaining
(check-equal? (remaining-apples 3 10 5) 2) ; 3 apples, 10 mins each, 5 mins passed, 2 remaining
(check-equal? (remaining-apples 8 0 50) 8) ; 8 apples, 0 mins each, 50 mins passed, 8 remaining
(check-equal? (remaining-apples 10 1 5) 5) ; 10 apples, 1 min each, 5 mins passed, 5 remaining
(check-equal? (remaining-apples 4 10 50) 0) ; 4 apples, 10 mins each, 50 mins passed, 0 remaining
(check-equal? (remaining-apples 7 3 20) 0)) ; 7 apples, 3 mins each, 20 mins passed, 0 remaining

(check remaining-apples)