#lang racket
(require rackunit)

(define (count-communicating-cows n d cow-positions)
  (let loop ([i 0] [count 0])
    (if (>= i (- n 1))
        count
        (let inner-loop ([j (+ i 1)] [current-count count])
          (if (>= j n)
              (loop (+ i 1) current-count)
              (let ([pos-i (list-ref cow-positions i)]
                    [pos-j (list-ref cow-positions j)])
                (if (<= (abs (- pos-i pos-j)) d)
                    (inner-loop (+ j 1) (+ current-count 1))
                    (inner-loop (+ j 1) current-count))))))))


(define (check count-communicating-cows)
(define tests
(list (check-equal? (count-communicating-cows 5 10 '(10 12 16 37 40)) 4)
(check-equal? (count-communicating-cows 4 5 '(1 3 6 10)) 4)
(check-equal? (count-communicating-cows 3 4 '(2 5 9)) 2)))
(andmap identity tests))

(check count-communicating-cows)