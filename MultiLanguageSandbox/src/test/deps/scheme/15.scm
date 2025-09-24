#lang racket
(require rackunit)

(define (find-majority-element lst)
  (let loop ((candidate #f)
             (count 0)
             (remaining lst))
    (cond
      ((null? remaining) candidate) ; Return the candidate if the list is empty
      ((zero? count)                ; If count is zero, set the current element as the new candidate
       (loop (car remaining) 1 (cdr remaining)))
      ((equal? candidate (car remaining)) ; If the current element matches the candidate, increment the count
       (loop candidate (+ count 1) (cdr remaining)))
      (else                         ; Otherwise, decrement the count
       (loop candidate (- count 1) (cdr remaining))))))

;; Test cases
(find-majority-element '(3 2 3))       ; => 3
(find-majority-element '(2 2 1 1 1 2 2)) ; => 2
(find-majority-element '(1))           ; => 1


(define (check find-majority-element)
(check-equal? (find-majority-element '(3 2 3)) 3)
(check-equal? (find-majority-element '(2 2 1 1 1 2 2)) 2)
(check-equal? (find-majority-element '(1)) 1)
(check-equal? (find-majority-element '(1 1 2 2 2 1 1)) 1)
(check-equal? (find-majority-element '(4 4 2 4)) 4))

(check find-majority-element)