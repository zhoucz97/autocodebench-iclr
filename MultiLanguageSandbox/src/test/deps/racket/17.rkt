#lang racket
(require rackunit)

(define (find-or-insert-index nums target)
  (let loop ([low 0]
             [high (- (length nums) 1)])
    (if (> low high)
        low
        (let* ([mid (quotient (+ low high) 2)]
               [mid-val (list-ref nums mid)])
          (cond
            [(= mid-val target) mid]
            [(< mid-val target) (loop (+ mid 1) high)]
            [else (loop low (- mid 1))])))))


(define (check find-or-insert-index)
(define tests
(list (check-equal? (find-or-insert-index (list 1 3 5 6) 5) 2)
(check-equal? (find-or-insert-index (list 1 3 5 6) 2) 1)
(check-equal? (find-or-insert-index (list 1 3 5 7) 6) 3)
(check-equal? (find-or-insert-index (list 1 3 4 6 8 10) 5) 3)
(check-equal? (find-or-insert-index '() 1) 0)))
(andmap identity tests))

(check find-or-insert-index)