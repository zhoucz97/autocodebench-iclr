#lang racket
(require rackunit)

(define (count-ones binary-string)
  (let loop ((i 0) (count 0))
    (if (= i 8)
        count
        (loop (+ i 1) 
              (if (char=? (string-ref binary-string i) #\1)
                  (+ count 1)
                  count)))))


(define (check count-ones)
(check-equal? (count-ones "01010101") 4)
(check-equal? (count-ones "00000000") 0)
(check-equal? (count-ones "11110000") 4)
(check-equal? (count-ones "00101011") 4)
(check-equal? (count-ones "10000001") 2)
(check-equal? (count-ones "01111110") 6))

(check count-ones)