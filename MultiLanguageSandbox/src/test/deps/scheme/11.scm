#lang racket
(require rackunit)

(define (add-one digits)
  (let loop ((digits digits) (carry 1))
    (if (null? digits)
        (if (= carry 1) '(1) '())
        (let* ((sum (+ (car digits) carry))
               (new-digit (modulo sum 10))
               (new-carry (quotient sum 10)))
          (cons new-digit (loop (cdr digits) new-carry))))))


(define (check add-one)
(check-equal? (add-one '(1 2 3)) '(1 2 4))
(check-equal? (add-one '(4 3 2 1)) '(4 3 2 2))
(check-equal? (add-one '(9 9 9)) '(1 0 0 0))
(check-equal? (add-one '(0)) '(1))
(check-equal? (add-one '(2 9 9)) '(3 0 0))
(check-equal? (add-one '(1 0 0 0)) '(1 0 0 1)))

(check add-one)