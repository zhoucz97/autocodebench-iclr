#lang racket
(require rackunit)

(define (unique-frequency-count arr)
  (let* ([freq (make-hash)]
         [counts (make-hash)])
    ; Count the frequency of each number
    (for-each (lambda (x) (hash-update freq x add1 0)) arr)
    ; Check if all frequency counts are unique
    (let loop ([keys (hash-keys freq)]
               [seen (make-hash)])
      (if (null? keys)
          #t
          (let ([count (hash-ref freq (car keys))])
            (if (hash-has-key? seen count)
                #f
                (begin
                  (hash-set seen count #t)
                  (loop (cdr keys) seen))))))))


;; Test Cases
(define (check unique-frequency-count)
(define tests
(list (check-equal? (unique-frequency-count (list 1 2 2 1 1 3)) #t)
(check-equal? (unique-frequency-count (list 1 2 2 3 3 3)) #t)
(check-equal? (unique-frequency-count (list 3 5 7 9 11)) #f)
(check-equal? (unique-frequency-count (list 4 4 4 4 4)) #t)
(check-equal? (unique-frequency-count (list 1 2 2 3 3 4 4 4)) #f)))
(andmap identity tests))

(check unique-frequency-count)