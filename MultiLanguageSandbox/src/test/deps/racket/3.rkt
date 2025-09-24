#lang racket
(require rackunit)

(define (is-boomerang points)
  (let* ([p1 (first points)]
         [p2 (second points)]
         [p3 (third points)]
         [x1 (first p1)]
         [y1 (second p1)]
         [x2 (first p2)]
         [y2 (second p2)]
         [x3 (first p3)]
         [y3 (second p3)])
    (and (not (and (= x1 x2) (= y1 y2)))
         (not (and (= x1 x3) (= y1 y3)))
         (not (and (= x2 x3) (= y2 y3)))
         (not (= (+ (* x1 (- y2 y3))
                    (* x2 (- y3 y1))
                    (* x3 (- y1 y2)))
                 0)))))


(define (check is-boomerang)
(define tests
(list (check-equal? (is-boomerang (list (list 1 1) (list 2 3) (list 3 2))) #t)
(check-equal? (is-boomerang (list (list 1 1) (list 2 2) (list 3 3))) #f)
(check-equal? (is-boomerang (list (list 0 0) (list 1 1) (list 1 0))) #t)
(check-equal? (is-boomerang (list (list 1 1) (list 1 1) (list 2 2))) #f)
(check-equal? (is-boomerang (list (list 1 2) (list 3 4) (list 5 6))) #f)))
(andmap identity tests))

(check is-boomerang)