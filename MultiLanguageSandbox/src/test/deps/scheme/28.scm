#lang racket
(require rackunit)

(define (max-calorie-value max-volume max-weight items)
  (let ((n (length items)))
    (let ((dp (make-vector (+ 1 n) (make-vector (+ 1 max-volume) (make-vector (+ 1 max-weight) 0)))))
      (do ((i 1 (+ i 1))) ((> i n))
        (let* ((item (list-ref items (- i 1)))
               (volume (car item))
               (weight (cadr item))
               (calories (caddr item)))
          (do ((v 0 (+ v 1))) ((> v max-volume))
            (do ((w 0 (+ w 1))) ((> w max-weight))
              (if (and (>= v volume) (>= w weight))
                  (let ((prev-v (- v volume))
                        (prev-w (- w weight)))
                    (vector-set (vector-ref (vector-ref dp i) v) w
                                 (max (vector-ref (vector-ref (vector-ref dp (- i 1)) v) w)
                                      (+ calories (vector-ref (vector-ref (vector-ref dp (- i 1)) prev-v) prev-w)))))
                  (vector-set (vector-ref (vector-ref dp i) v) w
                               (vector-ref (vector-ref (vector-ref dp (- i 1)) v) w)))))))
      (vector-ref (vector-ref (vector-ref dp n) max-volume) max-weight))))


(define (test-max-calorie-value)
(check-equal? (max-calorie-value 300 300 '((100 50 200) (200 100 300) (150 150 400))) 600)
(check-equal? (max-calorie-value 250 200 '((80 30 100) (60 70 150) (110 100 200))) 450)
(check-equal? (max-calorie-value 400 450 '((100 300 350) (150 100 400) (200 50 500))) 900)
(check-equal? (max-calorie-value 500 500 '((120 80 250) (130 90 300) (250 330 450))) 1000)
(check-equal? (max-calorie-value 200 150 '((50 30 100) (70 80 150) (80 40 200))) 450))

(test-max-calorie-value)