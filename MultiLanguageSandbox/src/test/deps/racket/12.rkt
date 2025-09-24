#lang racket
(require rackunit)

(define (extract-numbers-from-string str)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
    (cond
      [(null? chars) (if (null? current)
                         (reverse result)
                         (reverse (cons (string->number (list->string (reverse current))) result)))]
      [(char-numeric? (car chars))
       (loop (cdr chars) (cons (car chars) current) result)]
      [else
       (if (null? current)
           (loop (cdr chars) '() result)
           (loop (cdr chars) '() (cons (string->number (list->string (reverse current))) result)))])))


(define (check extract-numbers-from-string)
(define tests
(list (check-equal? (extract-numbers-from-string "abc123def456") '(123 456))
(check-equal? (extract-numbers-from-string "123abc45def") '(123 45))
(check-equal? (extract-numbers-from-string "no numbers") '())
(check-equal? (extract-numbers-from-string "100 300") '(100 300))
(check-equal? (extract-numbers-from-string "abc123xyz") '(123))))
(andmap identity tests))

(check extract-numbers-from-string)