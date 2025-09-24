#lang racket
(require rackunit)

(define (find-substring-index haystack needle)
  (let loop ([i 0])
    (cond
      [(> (+ i (string-length needle)) (string-length haystack)) -1]
      [(string=? (substring haystack i (+ i (string-length needle))) needle) i]
      [else (loop (+ i 1))])))


(define (check find-substring-index)
(define tests
(list (check-equal? (find-substring-index "hello world" "world") 6)
(check-equal? (find-substring-index "racket" "et") 4)
(check-equal? (find-substring-index "programming" "fun") -1)
(check-equal? (find-substring-index "hellohello" "lo") 3)
(check-equal? (find-substring-index "racket language" "guage") 10)
(check-equal? (find-substring-index "example" "exam") 0)))
(andmap identity tests))

(check find-substring-index)