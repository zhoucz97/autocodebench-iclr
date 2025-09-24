#lang racket
(require rackunit)

(define (count-title-characters title)
  (let loop ([chars (string->list title)] [count 0])
    (if (null? chars)
        count
        (let ([current-char (car chars)])
          (if (or (char-whitespace? current-char) (char=? current-char #\newline))
              (loop (cdr chars) count)
              (loop (cdr chars) (+ count 1)))))))


(define (check count-title-characters)
  (define tests
    (list (check-equal? (count-title-characters "234") 3)
          (check-equal? (count-title-characters "Ca 45") 4)
          (check-equal? (count-title-characters "Hello World") 10)
          (check-equal? (count-title-characters "Racket Programming 101\n") 20)
          (check-equal? (count-title-characters "   ") 0)))
  (andmap identity tests))

(check count-title-characters)