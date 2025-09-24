
#lang racket
(require rackunit)


(define (count-card-distributions n m card-distribution)
  (define mod 10007)
  
  ; Precompute factorial and inverse factorial arrays up to n modulo mod
  (define fact (make-vector (+ n 1) 1))
  (define inv-fact (make-vector (+ n 1) 1))
  
  ; Compute factorial[i] = i mod mod
  (for ([i (in-range 1 (+ n 1))])
    (vector-set fact i (modulo (* (vector-ref fact (- i 1)) i) mod)))
  
  ; Compute inverse factorial using Fermat's little theorem: inv(a) = a^(mod-2) mod mod
  (define (inv x)
    (define (exp b e)
      (cond [(= e 0) 1]
            [(even? e) (modulo (sqr (exp b (/ e 2))) mod)]
            [else (modulo (* b (exp b (- e 1))) mod)]))
    (exp x (- mod 2)))
  
  ; Compute inv-fact[i] = (i!)^(-1) mod mod
  (for ([i (in-range 1 (+ n 1))])
    (vector-set inv-fact i (inv (vector-ref fact i))))
  
  ; Check if the sum of card-distribution equals n
  (define sum-k (apply + card-distribution))
  (if (not (= sum-k n))
      0
      (let loop ([remaining n]
                 [result 1]
                 [dist card-distribution])
        (if (null? dist)
            result
            (let* ([k (car dist)]
                   [term (modulo (* result (vector-ref inv-fact k)) mod)])
              (loop (- remaining k) term (cdr dist)))))))


(define (check count-card-distributions)
(define tests
(list (check-equal? (count-card-distributions 5 2 '(3 1)) 20)
(check-equal? (count-card-distributions 20 19 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 8707)))
(andmap identity tests))

(check count-card-distributions)