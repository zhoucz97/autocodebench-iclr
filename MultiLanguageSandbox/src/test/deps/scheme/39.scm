#lang racket
(require rackunit)

(define (find-minimum-n k)
  (let loop ((n 1) (sum 0.0))
    (if (> sum k)
        (- n 1)
        (loop (+ n 1) (+ sum (/ 1.0 n))))))


(define (check find-minimum-n)
  (check-equal? (find-minimum-n 3) 11)
  (check-equal? (find-minimum-n 4) 31)
  (check-equal? (find-minimum-n 2) 4)
  (check-equal? (find-minimum-n 5) 83)
  (check-equal? (find-minimum-n 1) 2)
  (check-not-equal? (find-minimum-n 3) 10))

(check find-minimum-n)