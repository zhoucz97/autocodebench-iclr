
#lang racket
(require rackunit)


(define (factorial r)
  (if (<= r 1)
      1
      (* r (factorial (- r 1)))))

(define (stirling-second-kind n r)
  (cond
    [(and (= n 0) (= r 0)) 1]
    [(or (= n 0) (= r 0)) 0]
    [else (+ (* r (stirling-second-kind (- n 1) r))
             (stirling-second-kind (- n 1) (- r 1)))]))

(define (count-ball-arrangements n r)
  (* (stirling-second-kind n r) (factorial r)))


(define (check count-ball-arrangements)
(define tests
(list (check-equal? (count-ball-arrangements 3 2) 6)
(check-equal? (count-ball-arrangements 4 2) 14)
(check-equal? (count-ball-arrangements 5 3) 150)
(check-equal? (count-ball-arrangements 6 3) 540)
(check-equal? (count-ball-arrangements 5 5) 120)))
(andmap identity tests))

(check count-ball-arrangements)