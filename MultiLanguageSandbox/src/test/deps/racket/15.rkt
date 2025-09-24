#lang racket
(require rackunit)

(define (reverse-list lst)
  (if (null? lst)
      '()
      (append (reverse-list (cdr lst)) (list (car lst)))))


;; Test Cases
(define (check reverse-list)
(define tests
(list (check-equal? (reverse-list (list 1 2 3 4 5)) '(5 4 3 2 1))
(check-equal? (reverse-list (list 'a 'b 'c)) '(c b a))
(check-equal? (reverse-list '()) '())
(check-equal? (reverse-list (list 9)) '(9))
(check-equal? (reverse-list (list "hello" "world")) '("world" "hello"))))
(andmap identity tests))

(check reverse-list)