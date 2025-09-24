#lang racket
(require rackunit)

(define (find-element-over-quarter lst)
  (let* ([n (length lst)]
         [quarter (quotient n 4)])
    (cond
      [(> (count (lambda (x) (= x (list-ref lst quarter))) lst) quarter)
       (list-ref lst quarter)]
      [(> (count (lambda (x) (= x (list-ref lst (* 2 quarter)))) lst) quarter)
       (list-ref lst (* 2 quarter))]
      [(> (count (lambda (x) (= x (list-ref lst (* 3 quarter)))) lst) quarter)
       (list-ref lst (* 3 quarter))]
      [else (list-ref lst 0)]))) ; fallback (shouldn't be needed per problem statement)


;; Test Cases
(define (check find-element-over-quarter)
(define tests
(list (check-equal? (find-element-over-quarter (list 1 2 2 3 4 5 6)) 2)
(check-equal? (find-element-over-quarter (list 1 1 2 2 3)) 1)
(check-equal? (find-element-over-quarter (list 4 4 4 5 6 7 9 10)) 4)
(check-equal? (find-element-over-quarter (list 1 1 1 1 2 3 4)) 1)
(check-equal? (find-element-over-quarter (list 7 7 8 9 10 11 12)) 7)))
(andmap identity tests))

(check find-element-over-quarter)