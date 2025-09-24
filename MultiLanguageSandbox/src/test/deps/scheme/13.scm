#lang racket
(require rackunit)

(define (max-profit prices)
  (let loop ((prices prices) (min-price (car prices)) (max-profit 0))
    (if (null? prices)
        max-profit
        (let* ((current-price (car prices))
               (new-min (min min-price current-price))
               (potential-profit (- current-price min-price))
               (new-max (max max-profit potential-profit)))
          (loop (cdr prices) new-min new-max)))))


(define (check max-profit)
(check-equal? (max-profit '(7 1 5 3 6 4)) 5)
(check-equal? (max-profit '(7 6 4 3 1)) 0)
(check-equal? (max-profit '(1 2 3 4 5)) 4)
(check-equal? (max-profit '(1 1 1 1)) 0)
(check-equal? (max-profit '(2 4 1 5)) 4)
(check-equal? (max-profit '()) 0))

(check max-profit)