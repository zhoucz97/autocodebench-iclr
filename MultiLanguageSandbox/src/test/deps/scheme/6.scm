#lang racket
(require rackunit)

(define (length-of-last-word s)
  (let loop ((chars (string->list (string-trim-both s)))
             (count 0))
    (cond ((null? chars) count)
          ((char-whitespace? (car chars)) count)
          (else (loop (cdr chars) (+ count 1))))))


(define (check length-of-last-word)
(check-equal? (length-of-last-word "Hello World") 5)
(check-equal? (length-of-last-word " fly me to the moon ") 4)
(check-equal? (length-of-last-word "luffy is still joyboy") 6)
(check-equal? (length-of-last-word "single") 6)
(check-equal? (length-of-last-word " ") 0)
(check-equal? (length-of-last-word "") 0))

(check length-of-last-word)