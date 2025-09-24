
#lang racket
(require rackunit)


(define (last-stone-weight stones)
  (let loop ([stones (sort stones >)])
    (cond
      [(empty? stones) 0]  ; no stones left
      [(= (length stones) 1) (first stones)]  ; one stone remains
      [else
       (let* ([x (first stones)]
              [y (second stones)]
              [remaining (rest (rest stones))])
         (if (= x y)
             (loop remaining)  ; both stones are destroyed
             (loop (cons (- x y) remaining))))])))  ; smash stones and add result back


;; Test Cases
(define (check last-stone-weight)
(define tests
(list (check-equal? (last-stone-weight (list 2 7 4 1 8 1)) 1)
(check-equal? (last-stone-weight (list 10)) 10)
(check-equal? (last-stone-weight (list 8 10 4)) 2)
(check-equal? (last-stone-weight (list 20 15 10 5)) 0)
(check-equal? (last-stone-weight (list)) 0)))
(andmap identity tests))

(check last-stone-weight)