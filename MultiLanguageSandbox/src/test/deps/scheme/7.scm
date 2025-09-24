#lang racket
(require rackunit)

(define (add-binary a b)
  (let loop ((a (string->list a))
             (b (string->list b))
             (carry 0)
             (result '()))
    (cond ((and (null? a) (null? b))
           (if (= carry 0)
               (list->string (reverse result))
               (list->string (reverse (cons #\1 result)))))
          (else
           (let* ((digit-a (if (null? a) 0 (- (char->integer (car a)) 48)))
                  (digit-b (if (null? b) 0 (- (char->integer (car b)) 48)))
                  (sum (+ digit-a digit-b carry))
                  (new-digit (modulo sum 2))
                  (new-carry (quotient sum 2)))
             (loop (if (null? a) '() (cdr a))
                   (if (null? b) '() (cdr b))
                   new-carry
                   (cons (integer->char (+ new-digit 48)) result)))))))


(define (check add-binary)
(check-equal? (add-binary "11" "1") "100")
(check-equal? (add-binary "1010" "1011") "10101")
(check-equal? (add-binary "0" "0") "0")
(check-equal? (add-binary "111" "111") "1110")
(check-equal? (add-binary "1" "111") "1000")
(check-equal? (add-binary "1001" "1101") "10110"))

(check add-binary)