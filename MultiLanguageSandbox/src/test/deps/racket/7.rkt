
#lang racket
(require rackunit)


(define (sum-of-words-you-know words chars)
  (define (can-form-word? word char-counts)
    (let loop ([word word] [counts char-counts])
      (cond
        [(empty? word) #t]
        [else
         (let* ([char (string-ref word 0)]
                [new-counts (hash-update counts char (lambda (v) (- v 1)) 0)])
           (if (< (hash-ref new-counts char 0) 0)
               #f
               (loop (substring word 1) new-counts)))])))
  
  (define (count-chars s)
    (for/fold ([counts (hash)])
              ([char (in-string s)])
      (hash-update counts char add1 0)))
  
  (let ([char-counts (count-chars chars)])
    (apply + 
           (map (lambda (word)
                  (if (can-form-word? word char-counts)
                      (string-length word)
                      0))
                words))))


(define (check sum-of-words-you-know)
(define tests
(list (check-equal? (sum-of-words-you-know '("cat" "bt" "hat" "tree") "atach") 6)
(check-equal? (sum-of-words-you-know '("hello" "world" "leetcode") "welldonehoneyr") 10)
(check-equal? (sum-of-words-you-know '("apple" "orange" "banana") "aabbcc") 0)
(check-equal? (sum-of-words-you-know '("abc" "de" "fgh") "abcdefgh") 8)
(check-equal? (sum-of-words-you-know '("a" "b" "c") "abc") 3)))
(andmap identity tests))

(check sum-of-words-you-know)