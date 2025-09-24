#lang racket
(require rackunit)

(define (count-dictionary-lookups M N words)
  (let ((cache '()) (lookups 0))
    (define (update-cache word)
      (if (member word cache)
          (begin
            ; Move the word to the front (most recently used)
            (set cache (cons word (remove word cache))))
          (begin
            (set lookups (+ lookups 1))
            (if (>= (length cache) M)
                (set cache (cdr cache))) ; Evict the least recently used (last in list)
            (set cache (cons word cache)))))
    (for-each update-cache words)
    lookups))


(define (test-count-dictionary-lookups)
(check-equal? (count-dictionary-lookups 2 5 '(1 2 3 2 1)) 4)
(check-equal? (count-dictionary-lookups 3 6 '(4 4 4 5 6 7)) 4)
(check-equal? (count-dictionary-lookups 1 4 '(8 8 8 8)) 1)
(check-equal? (count-dictionary-lookups 2 3 '(9 9 10)) 2)
(check-equal? (count-dictionary-lookups 4 8 '(1 2 3 4 5 1 2 3)) 8))

(test-count-dictionary-lookups)