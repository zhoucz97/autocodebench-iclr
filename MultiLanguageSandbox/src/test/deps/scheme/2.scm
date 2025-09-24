#lang racket
(require rackunit)

(define (remove-duplicates nums)
  (let loop ((lst nums) (prev #f) (count 0))
    (cond ((null? lst) count)
          ((and prev (= (car lst) prev)) (loop (cdr lst) prev count))
          (else (loop (cdr lst) (car lst) (+ count 1))))))


(define (check remove-duplicates)
(check-equal? (remove-duplicates '(1 2 2 3)) 3)
(check-equal? (remove-duplicates '(1 1 1 1)) 1)
(check-equal? (remove-duplicates '(1 2 3 3 4)) 4)
(check-equal? (remove-duplicates '(4 4 4 4 4 4 4)) 1)
(check-equal? (remove-duplicates '(1 2 2 2 3 3 3 4 4 5 5 5 6)) 6)
(check-not-equal? (remove-duplicates '(1 2 3 4 5)) 4))

(check remove-duplicates)