#lang racket
(require rackunit)

(define (simulate-mock-exams n k m availability-list)
  (let ((exam-days (make-vector k 0))) ; Initialize a vector of size k with zeros
    (do ((i 0 (+ i 1))) ((= i m))
      (let loop ((student 0))
        (if (< student n)
            (let ((days (list-ref availability-list student)))
              (if (not (null? days))
                  (let ((day (car days)))
                    (vector-set exam-days (- day 1) (+ (vector-ref exam-days (- day 1)) 1))
                    (set availability-list (list-set availability-list student (cdr days))))
                  (loop (+ student 1))))
            #f)))
    (vector->list exam-days)))


(define (test-simulate-mock-exams)
(check-equal? (simulate-mock-exams 1 2 3 '((1 3))) '(1 0 1))
(check-equal? (simulate-mock-exams 2 2 4 '((1 4) (2 3))) '(1 1 1 1))
(check-equal? (simulate-mock-exams 3 3 5 '((1 2 5) (2 3 4) (3 4 5))) '(1 2 2 2 2)))

(test-simulate-mock-exams)