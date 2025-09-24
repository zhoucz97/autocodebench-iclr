#lang racket
(require rackunit)

(define (extract-digits str)
  (let loop ((chars (string->list str))
             (result '()))
    (if (null? chars)
        (list->string (reverse result))
        (let ((c (car chars)))
          (if (char-numeric? c)
              (loop (cdr chars) (cons c result))
              (loop (cdr chars) result))))))


(define (check extract-digits)
(check-equal? (extract-digits "hello123") "123")
(check-equal? (extract-digits "abc") "")
(check-equal? (extract-digits "2024code") "2024")
(check-equal? (extract-digits "123abc456") "123456")
(check-equal? (extract-digits "no digits here!") "")
(check-equal? (extract-digits "0") "0"))

(check extract-digits)