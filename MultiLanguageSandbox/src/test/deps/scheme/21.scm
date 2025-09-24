#lang racket
(require rackunit)

(define (minimum-nuts-to-defeat-zombies zombie-info)
  (let ((rows (make-vector 7 '()))) ; rows 1-6
    ; Group zombies by their row
    (for-each (lambda (zombie)
                (let ((row (car zombie))
                      (time (cadr zombie)))
                  (vector-set rows row (cons time (vector-ref rows row)))))
              zombie-info)
    ; Sort each row's times in ascending order
    (do ((i 1 (+ i 1)))
        ((> i 6))
      (vector-set rows i (sort (vector-ref rows i) <)))
    ; For each row, count the minimal nuts needed
    (let loop ((i 1)
               (total 0))
      (if (> i 6)
          total
          (let ((times (vector-ref rows i)))
            (if (null? times)
                (loop (+ i 1) total)
                (let ((nut-times '()))
                  (let inner-loop ((remaining times)
                                  (last-nut -1))
                    (if (null? remaining)
                        (loop (+ i 1) (+ total (length nut-times)))
                        (let ((current-time (car remaining)))
                          (if (or (= last-nut -1) (> current-time last-nut))
                              (inner-loop (cdr remaining) current-time)
                              (inner-loop (cdr remaining) last-nut))))))))))))


(define (test-minimum-nuts-to-defeat-zombies)
(check-equal? (minimum-nuts-to-defeat-zombies '((1 1) (2 2) (3 3))) 3)
(check-equal? (minimum-nuts-to-defeat-zombies '((1 1) (1 2) (1 3))) 1)
(check-equal? (minimum-nuts-to-defeat-zombies '((1 1) (2 2) (1 61))) 2)
(check-equal? (minimum-nuts-to-defeat-zombies '((1 1) (1 61) (2 1) (2 60) (3 2) (4 2) (5 3))) 5)
(check-equal? (minimum-nuts-to-defeat-zombies '((1 10) (2 20) (3 30) (4 40) (5 50) (6 60))) 6))

(test-minimum-nuts-to-defeat-zombies)