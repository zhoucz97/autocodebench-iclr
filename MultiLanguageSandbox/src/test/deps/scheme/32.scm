#lang racket
(require rackunit)

(define (find-test-answers exam-data student-answers)
  (map (lambda (student-answer)
         (let* ((question (car student-answer))
                (options (cdr student-answer))
                (correct-answer (cadr (assoc question exam-data)))
                (index (position correct-answer options)))
           (cond ((= index 0) "A")
                 ((= index 1) "B")
                 ((= index 2) "C")
                 ((= index 3) "D")
                 (else ""))))
       student-answers))

; Helper function to find the position of an element in a list
(define (position item lst)
  (let loop ((lst lst) (i 0))
    (cond ((null? lst) -1)
          ((equal? item (car lst)) i)
          (else (loop (cdr lst) (+ i 1))))))


(define (check-find-test-answers)
(check-equal? 
(find-test-answers '(("math" "C") ("physics" "A"))
'(("math" "B" "C" "D" "A")
("physics" "A" "B" "C" "D")))
'("B" "A"))

(check-equal?
(find-test-answers '(("history" "D"))
'(("history" "A" "B" "C" "D")))
'("D"))

(check-equal?
(find-test-answers '(("geography" "A") ("biology" "B"))
'(("geography" "A" "B" "C" "D")
("biology" "C" "D" "A" "B")))
'("A" "D")))

(check-find-test-answers)