#lang racket
(require rackunit)

(define (find-word-and-count word text)
  (let* ([lower-word (string-downcase word)]
         [words (string-split text)]
         [lower-words (map string-downcase words)]
         [matches (filter (lambda (w) (string=? w lower-word)) lower-words)]
         [count (length matches)]
         [first-pos (let loop ([lst words] [pos 0])
                      (cond
                        [(null? lst) -1]
                        [(string=? (string-downcase (car lst)) lower-word) pos]
                        [else (loop (cdr lst) (+ pos 1))]))])
    (if (> count 0)
        (list count first-pos)
        -1)))


(define (check find-word-and-count)
(define tests
(list (check-equal? (find-word-and-count "To" "to be or not to be is a question") '(2 0))
(check-equal? (find-word-and-count "to" "Did the Ottoman Empire lose its power at that time") -1)
(check-equal? (find-word-and-count "a" "A man a plan a canal Panama") '(3 0))))
(andmap identity tests))

(check find-word-and-count)