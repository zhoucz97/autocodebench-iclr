#lang racket
(require rackunit)

(define (is-leap-year year)
  (cond
    ((= (modulo year 400) 0) 1)
    ((= (modulo year 100) 0) 0)
    ((= (modulo year 4) 0) 1)
    (else 0)))


(define (check is-leap-year)
(check-equal? (is-leap-year 2024) 1)
(check-equal? (is-leap-year 2100) 0)
(check-equal? (is-leap-year 2004) 1)
(check-equal? (is-leap-year 1901) 0)
(check-equal? (is-leap-year 2400) 1)
(check-equal? (is-leap-year 1999) 0))

(check is-leap-year)