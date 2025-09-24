#lang racket
(require rackunit)

(define (rearrange-numbers numbers order)
  (let* ((sorted (sort numbers <))
         (a (first sorted))
         (b (second sorted))
         (c (third sorted)))
    (map (lambda (char)
           (cond
             [(char=? char #\A) a]
             [(char=? char #\B) b]
             [(char=? char #\C) c]))
         (string->list order))))


(define (check rearrange-numbers)
(define tests
(list (check-equal? (rearrange-numbers (list 8 3 5) "BCA") '(5 8 3))
(check-equal? (rearrange-numbers (list 10 20 15) "CAB") '(20 10 15))
(check-equal? (rearrange-numbers (list 7 9 8) "ACB") '(7 9 8))
(check-equal? (rearrange-numbers (list 12 18 14) "BAC") '(14 12 18))
(check-equal? (rearrange-numbers (list 6 2 4) "CBA") '(6 4 2))))
(andmap identity tests))

(check rearrange-numbers)