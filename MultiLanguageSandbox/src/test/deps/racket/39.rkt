#lang racket
(require rackunit)

(define (max-herb-value T M herb-data)
  (let ((dp (make-vector (+ M 1) (make-vector (+ T 1) 0))))
    (for ([i (in-range 1 (+ M 1))])
      (let* ((current-herb (list-ref herb-data (- i 1)))
             (time (car current-herb))
             (value (cadr current-herb)))
        (for ([j (in-range 1 (+ T 1))])
          (if (>= j time)
              (vector-set (vector-ref dp i) j
                           (max (vector-ref (vector-ref dp (- i 1)) j)
                                (+ (vector-ref (vector-ref dp (- i 1)) (- j time)) value)))
              (vector-set (vector-ref dp i) j
                           (vector-ref (vector-ref dp (- i 1)) j))))))
    (vector-ref (vector-ref dp M) T)))


(define (test-max-herb-value)
(define tests
(list (check-equal? (max-herb-value 10 2 '((5 10) (4 7))) 17)
(check-equal? (max-herb-value 10 3 '((3 6) (4 7) (2 4))) 17)
(check-equal? (max-herb-value 15 4 '((3 8) (5 15) (6 10) (7 6))) 33)
(check-equal? (max-herb-value 5 2 '((2 3) (3 4))) 7)
(check-equal? (max-herb-value 7 3 '((1 2) (2 5) (3 7))) 14)))
(andmap identity tests))

(test-max-herb-value)