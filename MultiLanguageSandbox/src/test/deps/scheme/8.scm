#lang racket
(require rackunit)

(define (is-palindrome s)
  (let* ((lower-str (string-downcase s))
         (cleaned-str (list->string 
                       (filter (lambda (c) 
                                 (or (char-alphabetic? c) 
                                     (char-numeric? c)))
                               (string->list lower-str)))))
    (let ((len (string-length cleaned-str)))
      (let loop ((i 0) (j (- len 1)))
        (cond ((>= i j) #t)
              ((not (char=? (string-ref cleaned-str i)
                            (string-ref cleaned-str j))) #f)
              (else (loop (+ i 1) (- j 1))))))))


(define (check is-palindrome)
(check-equal? (is-palindrome "A man, a plan, a canal: Panama") #t)
(check-equal? (is-palindrome "race a car") #f)
(check-equal? (is-palindrome " ") #t)
(check-equal? (is-palindrome "No lemon, no melon") #t)
(check-equal? (is-palindrome "Red rum, sir, is murder") #t)
(check-equal? (is-palindrome "Was it a car or a cat I saw") #t)
(check-equal? (is-palindrome "Madam, in Eden, I'm Adam") #t)
(check-not-equal? (is-palindrome "Example") #t))

(check is-palindrome)