#lang racket
(require rackunit)

(define (height-mismatch-count heights expected)
  (let loop ([i 0] [count 0])
    (if (= i (length heights))
        count
        (loop (+ i 1) 
              (if (not (= (list-ref heights i) (list-ref expected i)))
                  (+ count 1)
                  count)))))


(define (check height-mismatch-count)
(define tests
(list (check-equal? (height-mismatch-count (list 1 1 4 2 1 3) (list 1 1 1 2 3 4)) 3)
(check-equal? (height-mismatch-count (list 5 1 2 3 4) (list 1 2 3 4 5)) 5)
(check-equal? (height-mismatch-count (list 1 2 3 4 5) (list 1 2 3 4 5)) 0)
(check-equal? (height-mismatch-count (list 3 3 3 3 3) (list 3 3 3 3 3)) 0)
(check-equal? (height-mismatch-count (list 2 1 2 1 2) (list 1 1 2 2 2)) 2)))
(andmap identity tests))

(check height-mismatch-count)