#lang racket
(require rackunit)

(define (can-partition-into-three-equals arr)
  (let* ((total-sum (apply + arr))
         (target (/ total-sum 3)))
    (if (not (integer? target))
        #f
        (let loop ((i 0)
                   (current-sum 0)
                   (parts-found 0))
          (cond
            [(= parts-found 2) #t]
            [(>= i (length arr)) #f]
            [else
             (let ((new-sum (+ current-sum (list-ref arr i))))
               (if (= new-sum (* (+ parts-found 1) target))
                   (loop (+ i 1) 0 (+ parts-found 1))
                   (loop (+ i 1) new-sum parts-found)))])))))


(define (check can-partition-into-three-equals)
(define tests
(list (check-equal? (can-partition-into-three-equals (list 0 2 1 -6 6 -7 9 1 2 0 1)) #t)
(check-equal? (can-partition-into-three-equals (list 0 2 1 -6 6 7 9 -1 2 0 1)) #f)
(check-equal? (can-partition-into-three-equals (list 3 3 3 3 3 3)) #t)
(check-equal? (can-partition-into-three-equals (list 1 1 1 1)) #f)
(check-equal? (can-partition-into-three-equals (list 1 -1 1 -1)) #f)))
(andmap identity tests))

(check can-partition-into-three-equals)