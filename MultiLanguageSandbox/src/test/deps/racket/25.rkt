#lang racket
(require rackunit)

(define (find-aron-position colors)
  (if (null? colors)
      0
      (let loop ([remaining (cdr colors)]
                 [prev (car colors)]
                 [count 1])
        (if (null? remaining)
            count
            (let ([current (car remaining)])
              (if (equal? current prev)
                  (loop (cdr remaining) prev count)
                  (loop (cdr remaining) current (+ count 1))))))))


(define (check find-aron-position)
(define tests
(list (check-equal? (find-aron-position '("R" "G" "B" "B" "Y")) 5)
(check-equal? (find-aron-position '("B" "B" "R" "G" "G" "Y")) 5)
(check-equal? (find-aron-position '("Y" "Y" "Y")) 2)
(check-equal? (find-aron-position '("B" "G" "G" "R" "R")) 4)
(check-equal? (find-aron-position '("R" "R" "R" "B" "B" "B" "G" "G")) 4)))
(andmap identity tests))

(check find-aron-position)