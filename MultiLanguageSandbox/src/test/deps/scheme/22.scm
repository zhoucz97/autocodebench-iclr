#lang racket
(require rackunit)

(define (max-untouched-nodes n)
  (if (< n 2)
      n
      (quotient (+ n 1) 3)))


(define (check max-untouched-nodes)
(check-equal? (max-untouched-nodes 2) 2)
(check-equal? (max-untouched-nodes 3) 1)
(check-equal? (max-untouched-nodes 4) 2)
(check-equal? (max-untouched-nodes 5) 1)
(check-equal? (max-untouched-nodes 6) 2)
(check-equal? (max-untouched-nodes 7) 1)
(check-equal? (max-untouched-nodes 8) 2)
(check-equal? (max-untouched-nodes 9) 1))

(check max-untouched-nodes)