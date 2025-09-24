#lang racket
(require rackunit)

(define (merge-arrays nums1 m nums2 n)
  (let loop ([i (- m 1)]  ; index for nums1 (non-zero elements)
             [j (- n 1)]  ; index for nums2
             [k (- (+ m n) 1)])  ; current position in merged array
    (cond
      [(< j 0) nums1]  ; if all nums2 elements are processed, return nums1
      [else
       (if (and (>= i 0) (> (list-ref nums1 i) (list-ref nums2 j)))
           (begin
             (list-set nums1 k (list-ref nums1 i))
             (loop (- i 1) j (- k 1)))
           (begin
             (list-set nums1 k (list-ref nums2 j))
             (loop i (- j 1) (- k 1))))])))


(define (check merge-arrays)
(define tests
(list (check-equal? (merge-arrays (list 1 2 3 0 0 0) 3 (list 2 5 6) 3) '(1 2 2 3 5 6))
(check-equal? (merge-arrays (list 1) 1 '() 0) '(1))
(check-equal? (merge-arrays '() 0 (list 1 2 3) 3) '(1 2 3))
(check-equal? (merge-arrays (list 4 5 6 0 0 0) 3 (list 1 2 3) 3) '(1 2 3 4 5 6))
(check-equal? (merge-arrays (list 2 0) 1 (list 1) 1) '(1 2))))
(andmap identity tests))

(check merge-arrays)