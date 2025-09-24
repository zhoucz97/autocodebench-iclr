#lang racket
(require rackunit)

(define (find-missing-number lst)
  (let* ((n (length lst))
         (expected-sum (* n (+ n 1) 1/2))
         (actual-sum (apply + lst)))
    (- expected-sum actual-sum)))


(define (check find-missing-number)
(check-equal? (find-missing-number '(3 0 1)) 2)
(check-equal? (find-missing-number '(0 1)) 2)
(check-equal? (find-missing-number '(9 6 4 2 3 5 7 0 1)) 8)
(check-equal? (find-missing-number '(0)) 1)
(check-equal? (find-missing-number '(1 2 3 4 5 6 7 8 9)) 0))

(check find-missing-number)