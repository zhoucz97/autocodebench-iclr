#lang racket
(require rackunit)

(define (replace-elements-with-ranks arr)
  (let* ([sorted-unique (sort (remove-duplicates arr) <)]
         [rank-map (make-hash)])
    ; Create a hash map from value to rank
    (for ([val sorted-unique]
          [rank (in-naturals 1)])
      (hash-set rank-map val rank))
    ; Replace each element with its rank
    (map (lambda (x) (hash-ref rank-map x)) arr)))


(define (check replace-elements-with-ranks)
(define tests
(list (check-equal? (replace-elements-with-ranks (list 40 10 20 30)) '(4 1 2 3))
(check-equal? (replace-elements-with-ranks (list 100 100 100)) '(1 1 1))
(check-equal? (replace-elements-with-ranks (list 5 5 6 7)) '(1 1 2 3))
(check-equal? (replace-elements-with-ranks (list 10 20 20 30)) '(1 2 2 3))
(check-equal? (replace-elements-with-ranks (list 7 5 6 7)) '(3 1 2 3))))
(andmap identity tests))

(check replace-elements-with-ranks)