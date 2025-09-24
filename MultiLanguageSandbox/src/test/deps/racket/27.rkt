#lang racket
(require rackunit)

(define (can-fit-sticks-in-boxes sticks boxes)
  (let ((sorted-sticks (sort sticks <))
        (sorted-boxes (sort boxes <)))
    (if (= (length sticks) (length boxes))
        (let loop ((s sorted-sticks) (b sorted-boxes))
          (cond
            [(null? s) "DA"]
            [(> (car s) (car b)) "NE"]
            [else (loop (cdr s) (cdr b))]))
        "NE")))


(define (check can-fit-sticks-in-boxes)
(define tests
(list (check-equal? (can-fit-sticks-in-boxes '(5 4 6) '(6 5 7)) "DA")
(check-equal? (can-fit-sticks-in-boxes '(8 6 7) '(5 4 3)) "NE")
(check-equal? (can-fit-sticks-in-boxes '(2 3 4) '(1 5 3)) "NE")
(check-equal? (can-fit-sticks-in-boxes '(1 1 1) '(2 2 2)) "DA")
(check-equal? (can-fit-sticks-in-boxes '(10 20 30) '(30 20 10)) "DA")))
(andmap identity tests))

(check can-fit-sticks-in-boxes)