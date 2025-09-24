#lang racket
(require rackunit)

(define (move-zeros-to-end lst)
  (let loop ((remaining lst) (non-zeros '()) (zeros 0))
    (cond ((null? remaining)
           (append (reverse non-zeros) (make-list zeros 0)))
          ((zero? (car remaining))
           (loop (cdr remaining) non-zeros (+ zeros 1)))
          (else
           (loop (cdr remaining) (cons (car remaining) non-zeros) zeros)))))


(define (check move-zeros-to-end)
(check-equal? (move-zeros-to-end '(0 1 0 3 12)) '(1 3 12 0 0))
(check-equal? (move-zeros-to-end '(0 0 1)) '(1 0 0))
(check-equal? (move-zeros-to-end '(1 0 2 0 3)) '(1 2 3 0 0))
(check-equal? (move-zeros-to-end '(0 0 0)) '(0 0 0))
(check-equal? (move-zeros-to-end '(1 2 3)) '(1 2 3))
(check-equal? (move-zeros-to-end '()) '()))

(check move-zeros-to-end)