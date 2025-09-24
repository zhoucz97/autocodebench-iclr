#lang racket
(require rackunit)

(define (contains-duplicate lst)
  (let loop ((seen '()) (remaining lst))
    (cond ((null? remaining) #f)
          ((member (car remaining) seen) #t)
          (else (loop (cons (car remaining) seen) (cdr remaining))))))


(define (check contains-duplicate)
(check-equal? (contains-duplicate '(1 2 3 1)) #t)
(check-equal? (contains-duplicate '(1 2 3 4)) #f)
(check-equal? (contains-duplicate '(1 1 1 3 3 4 3 2 4 2)) #t)
(check-equal? (contains-duplicate '(1)) #f)
(check-equal? (contains-duplicate '(1 2 2)) #t)
(check-equal? (contains-duplicate '()) #f))

(check contains-duplicate)