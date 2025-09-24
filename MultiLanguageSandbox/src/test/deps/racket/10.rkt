#lang racket
(require rackunit)

(define (min-cost-to-move-chips position)
  (let loop ([positions position]
             [even-count 0]
             [odd-count 0])
    (if (null? positions)
        (min even-count odd-count)
        (let ([current-pos (car positions)])
          (if (even? current-pos)
              (loop (cdr positions) (+ even-count 1) odd-count)
              (loop (cdr positions) even-count (+ odd-count 1)))))))


;; Test Cases
(define (check min-cost-to-move-chips)
(define tests
(list (check-equal? (min-cost-to-move-chips (list 1 2 3)) 1)
(check-equal? (min-cost-to-move-chips (list 2 2 2 3 3)) 2)
(check-equal? (min-cost-to-move-chips (list 1 3 5 7 9)) 0)
(check-equal? (min-cost-to-move-chips (list 10 3 3 3)) 1)
(check-equal? (min-cost-to-move-chips (list 6 4 7 8 2)) 1)))
(andmap identity tests))

(check min-cost-to-move-chips)