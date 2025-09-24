#lang racket
(require rackunit)

(define (array-intersection lst1 lst2)
  (let ((set1 (list->vector (sort (remove-duplicates lst1) <)))
        (set2 (list->vector (sort (remove-duplicates lst2) <))))
    (let loop ((i 0) (j 0) (result '()))
      (cond ((or (>= i (vector-length set1)) (>= j (vector-length set2))) 
             (reverse result))
            ((= (vector-ref set1 i) (vector-ref set2 j))
             (loop (+ i 1) (+ j 1) (cons (vector-ref set1 i) result)))
            ((< (vector-ref set1 i) (vector-ref set2 j))
             (loop (+ i 1) j result))
            (else
             (loop i (+ j 1) result))))))


(define (check array-intersection)
(check-equal? (array-intersection '(1 2 2 1) '(2 2)) '(2))
(check-equal? (array-intersection '(4 9 5) '(9 4 9 8 4)) '(4 9))
(check-equal? (array-intersection '(1 2 3) '(4 5 6)) '())
(check-equal? (array-intersection '(1 3 5 7) '(2 4 6 8)) '())
(check-equal? (array-intersection '(1 1 1 1) '(1)) '(1))
(check-equal? (array-intersection '() '(1 2 3)) '()))

(check array-intersection)