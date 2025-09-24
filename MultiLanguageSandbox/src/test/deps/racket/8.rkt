#lang racket
(require rackunit)

(define (shortest-distance-in-bus-route distance start dest)
  (let* ([n (length distance)]
         [total-distance (apply + distance)]
         [clockwise-distance (if (< start dest)
                                 (apply + (take distance (- dest start)))
                                 (apply + (append (drop distance start) (take distance dest))))]
         [counterclockwise-distance (- total-distance clockwise-distance)])
    (min clockwise-distance counterclockwise-distance)))


;; Test Cases
(define (check shortest-distance-in-bus-route)
(define tests
(list (check-equal? (shortest-distance-in-bus-route (list 1 2 3 4) 0 3) 4)
(check-equal? (shortest-distance-in-bus-route (list 1 2 3 4) 0 2) 3)
(check-equal? (shortest-distance-in-bus-route (list 1 2 3 4 5) 3 1) 5)
(check-equal? (shortest-distance-in-bus-route (list 7 10 1 12 11 14 5 0) 7 2) 17)
(check-equal? (shortest-distance-in-bus-route (list 1 2 3 4 5) 2 2) 0)))
(andmap identity tests))

(check shortest-distance-in-bus-route)