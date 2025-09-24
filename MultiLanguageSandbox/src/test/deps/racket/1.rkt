#lang racket
(require rackunit)

(define (has-close-elements lst threshold)
  (let loop ([remaining (cdr lst)] [prev (car lst)])
    (cond
      [(null? remaining) #f]
      [(< (abs (- (car remaining) prev)) threshold) #t]
      [else (loop (cdr remaining) (car remaining))])))


;; Test cases
(define (check has-close-elements)
(define tests
(list (check-equal? (has-close-elements (list 1.0 2.0 3.9 4.0 5.0 2.2) 0.3) #t)
(check-equal? (has-close-elements (list 1.0 2.0 3.9 4.0 5.0 2.2) 0.05) #f)
(check-equal? (has-close-elements (list 1.0 2.0 5.9 4.0 5.0) 0.95) #t)
(check-equal? (has-close-elements (list 1.0 2.0 5.9 4.0 5.0) 0.8) #f)
(check-equal? (has-close-elements (list 1.0 2.0 3.0 4.0 5.0 2.0) 0.1) #t)
(check-equal? (has-close-elements (list 1.1 2.2 3.1 4.1 5.1) 1.0) #t)
(check-equal? (has-close-elements (list 1.1 2.2 3.1 4.1 5.1) 0.5) #f)))
(andmap identity tests))

(check has-close-elements)