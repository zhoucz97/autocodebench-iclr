#lang racket
(require rackunit)

(define (is-strictly-monotonic lst)
  (cond
    [(null? lst) #t]  ; empty list is considered strictly monotonic
    [(null? (cdr lst)) #t]  ; single-element list is strictly monotonic
    [else
     (let loop ([prev (car lst)]
                [rest (cdr lst)]
                [increasing #f]
                [decreasing #f])
       (cond
         [(null? rest) #t]
         [else
          (let ([current (car rest)])
            (cond
              [(and (not increasing) (not decreasing))
               (if (< prev current)
                   (loop current (cdr rest) #t #f)
                   (if (> prev current)
                       (loop current (cdr rest) #f #t)
                       #f))]  ; equal elements make it not strictly monotonic
              [(and increasing (> prev current)) #f]
              [(and decreasing (< prev current)) #f]
              [else (loop current (cdr rest) increasing decreasing)]))]))]))


(define (check is-strictly-monotonic)
(define tests
(list (check-equal? (is-strictly-monotonic (list 1 2 3 4 5)) #t)
(check-equal? (is-strictly-monotonic (list 5 4 3 2 1)) #t)
(check-equal? (is-strictly-monotonic (list 1 2 2 3 4)) #f)
(check-equal? (is-strictly-monotonic (list 1)) #t)
(check-equal? (is-strictly-monotonic (list 3 2 1 2 3)) #f)))
(andmap identity tests))

(check is-strictly-monotonic)