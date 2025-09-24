
#lang racket
(require rackunit)


(define (count-hehe matrix)
  (let* ([rows (length matrix)]
         [cols (if (null? matrix) 0 (length (car matrix)))]
         [directions '((-1 -1) (-1 0) (-1 1)
                       (0 -1)          (0 1)
                       (1 -1)  (1 0)  (1 1))])
    (let loop ([i 0] [j 0] [count 0])
      (cond
        [(>= i rows) count]
        [(>= j cols) (loop (add1 i) 0 count)]
        [else
         (let ([current-char (list-ref (list-ref matrix i) j)])
           (if (char=? current-char #\h)
               (let ([new-count (for/sum ([dir directions])
                                  (let ([di (car dir)]
                                        [dj (cadr dir)])
                                    (if (and (>= (+ i di) 0) (< (+ i di) rows)
                                             (>= (+ j dj) 0) (< (+ j dj) cols)
                                             (char=? (list-ref (list-ref matrix (+ i di)) (+ j dj)) #\e))
                                        (let ([di2 (* 2 di)]
                                              [dj2 (* 2 dj)])
                                          (if (and (>= (+ i di2) 0) (< (+ i di2) rows)
                                                   (>= (+ j dj2) 0) (< (+ j dj2) cols)
                                                   (char=? (list-ref (list-ref matrix (+ i di2)) (+ j dj2)) #\h))
                                              (let ([di3 (* 3 di)]
                                                    [dj3 (* 3 dj)])
                                                (if (and (>= (+ i di3) 0) (< (+ i di3) rows)
                                                         (>= (+ j dj3) 0) (< (+ j dj3) cols)
                                                         (char=? (list-ref (list-ref matrix (+ i di3)) (+ j dj3)) #\e))
                                                    1
                                                    0))
                                              0))
                                        0)))]
                 (loop i (add1 j) (+ count new-count))))
               (loop i (add1 j) count)))])))))


(define (check count-hehe)
(define tests
(list (check-equal? (count-hehe '(("h" "e" "h" "e" "h") ("e" "h" "e" "h" "e") ("h" "e" "h" "e" "h") ("e" "h" "e" "h" "e") ("h" "e" "h" "e" "h"))) 10)
(check-equal? (count-hehe '(("h" "e" "l" "l" "o") ("e" "h" "e" "h" "e") ("h" "e" "l" "l" "o"))) 1)))
(andmap identity tests))

(check count-hehe)