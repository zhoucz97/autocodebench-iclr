#lang racket
(require rackunit)

(define (find-substring haystack needle)
  (let ((haystack-len (string-length haystack))
        (needle-len (string-length needle)))
    (if (or (= needle-len 0) (> needle-len haystack-len))
        -1
        (let loop ((i 0))
          (if (> (+ i needle-len) haystack-len)
              -1
              (if (string=? (substring haystack i (+ i needle-len)) needle)
                  i
                  (loop (+ i 1))))))))


(define (check find-substring)
(check-equal? (find-substring "hello" "ll") 2)
(check-equal? (find-substring "aaaaa" "bba") -1)
(check-equal? (find-substring "abc" "c") 2)
(check-equal? (find-substring "haystack" "stack") 3)
(check-equal? (find-substring "needle" "needle") 0)
(check-equal? (find-substring "short" "longneedle") -1))

(check find-substring)