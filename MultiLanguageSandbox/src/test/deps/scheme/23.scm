#lang racket
(require rackunit)

(define (w a b c)
  (let ((memo (make-hash)))
    (define (helper a b c)
      (let ((key (list a b c)))
        (if (hash-has-key? memo key)
            (hash-ref memo key)
            (let ((result
                   (cond
                    ((or (<= a 0) (<= b 0) (<= c 0)) 1)
                    ((or (> a 20) (> b 20) (> c 20)) (helper 20 20 20))
                    ((and (< a b) (< b c))
                     (+ (helper a b (- c 1))
                        (helper a (- b 1) (- c 1))
                        (- (helper a (- b 1) c))))
                    (else
                     (+ (helper (- a 1) b c)
                        (helper (- a 1) (- b 1) c)
                        (helper (- a 1) b (- c 1))
                        (- (helper (- a 1) (- b 1) (- c 1))))))))
              (hash-set memo key result)
              result))))
    (helper a b c)))


(define (test-w)
(check-equal? (w 0 0 0) 1)
(check-equal? (w 21 21 21) 1048576)
(check-equal? (w 10 10 10) 1024)
(check-equal? (w 5 5 5) 32)
(check-equal? (w 15 15 15) 32768)
(check-equal? (w 20 20 20) 1048576))

(test-w)