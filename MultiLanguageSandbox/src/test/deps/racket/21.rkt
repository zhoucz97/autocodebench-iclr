#lang racket
(require rackunit)

(define (sum-leap-years start-year end-year)
  (define (leap-year? year)
    (or (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))
        (= (modulo year 400) 0)))
  (let loop ([year (+ start-year 1)] [sum 0])
    (if (>= year (- end-year 1))
        sum
        (loop (+ year 1) (if (leap-year? year) (+ sum year) sum)))))


(define (check sum-leap-years)
(define tests
(list (check-equal? (sum-leap-years 2000 2005) 2004)
(check-equal? (sum-leap-years 1980 1990) 3972)
(check-equal? (sum-leap-years 1990 2000) 5988)
(check-equal? (sum-leap-years 1900 2001) 48800)
(check-equal? (sum-leap-years 2001 2010) 4012)
(check-equal? (sum-leap-years 1800 1900) 44400)))
(andmap identity tests))

(check sum-leap-years)