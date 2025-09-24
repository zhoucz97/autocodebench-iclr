#lang racket
(require rackunit)

(define (determine-winner stone-piles)
  (let loop ([piles stone-piles] [prev 0] [moves 0])
    (if (null? piles)
        (if (odd? moves) "Alice" "Bob")
        (let* ([current (car piles)]
               [new-moves (+ moves (- current prev))])
          (loop (cdr piles) current new-moves)))))


(define (check determine-winner)
(define tests
(list (check-equal? (determine-winner '(1)) "Alice")
(check-equal? (determine-winner '(2 3)) "Alice")
(check-equal? (determine-winner '(1 2 3)) "Bob")
(check-equal? (determine-winner '(2)) "Bob")
(check-equal? (determine-winner '(1 2 3 4)) "Bob")))
(andmap identity tests))

(check determine-winner)