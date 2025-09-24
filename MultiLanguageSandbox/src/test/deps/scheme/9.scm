#lang racket
(require rackunit)

(define (number-to-column n)
  (let loop ((n (- n 1)) (result '()))
    (if (< n 0)
        (list->string result)
        (loop (quotient n 26) 
              (cons (integer->char (+ (remainder n 26) (char->integer #\A))) 
                    result)))))


(define (check number-to-column)
(check-equal? (number-to-column 1) "A")
(check-equal? (number-to-column 28) "AB")
(check-equal? (number-to-column 701) "ZY")
(check-equal? (number-to-column 52) "AZ")
(check-equal? (number-to-column 705) "AAC")
(check-equal? (number-to-column 26) "Z")
(check-equal? (number-to-column 27) "AA"))

(check number-to-column)