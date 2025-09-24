#lang racket
(require rackunit)

(define (duplicate-zeros arr)
  (let loop ([i 0] [n (length arr)])
    (when (< i n)
      (if (zero? (list-ref arr i))
          (begin
            ; Shift elements to the right starting from i+1
            (for ([j (in-range (- n 1) i -1)])
              (list-set arr (+ j 1) (list-ref arr j)))
            ; Set the next position to 0
            (list-set arr (+ i 1) 0)
            ; Skip the next position since we just duplicated the zero
            (loop (+ i 2) n))
          (loop (+ i 1) n)))))


;; Test Cases
(define (check duplicate-zeros)
(define tests
(list (check-equal? (duplicate-zeros (list 1 0 2 3 0 4 5 0)) (list 1 0 0 2 3 0 0 4))
(check-equal? (duplicate-zeros (list 1 2 3)) (list 1 2 3))
(check-equal? (duplicate-zeros (list 0 0 0 0 0 0)) (list 0 0 0 0 0 0))
(check-equal? (duplicate-zeros (list 0 1 7 6 0)) (list 0 0 1 7 6))
(check-equal? (duplicate-zeros (list 8 4 5 0 0 0 0 7)) (list 8 4 5 0 0 0 0 0))))
(andmap identity tests))

(check duplicate-zeros)