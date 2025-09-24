#lang racket
(require rackunit)

(define (merge-arrays arr1 arr2)
  (cond ((null? arr1) arr2)
        ((null? arr2) arr1)
        ((<= (car arr1) (car arr2))
         (cons (car arr1) (merge-arrays (cdr arr1) arr2)))
        (else
         (cons (car arr2) (merge-arrays arr1 (cdr arr2))))))


(define (check merge-arrays)
(check-equal? (merge-arrays '(1 2 4) '(1 3 4)) '(1 1 2 3 4 4))
(check-equal? (merge-arrays '(1 3 5 7) '(2 4 6 8)) '(1 2 3 4 5 6 7 8))
(check-equal? (merge-arrays '(1 1 1) '(2 2 2)) '(1 1 1 2 2 2))
(check-equal? (merge-arrays '() '(1 2 3)) '(1 2 3))
(check-equal? (merge-arrays '(4 5 6) '()) '(4 5 6))
(check-equal? (merge-arrays '() '()) '()))

(check merge-arrays)