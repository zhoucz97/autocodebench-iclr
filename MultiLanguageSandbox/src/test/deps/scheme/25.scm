#lang racket
(require rackunit)

(define (max-apples-collected apple-data chair-height tao-max-reach init-energy)
  (let* ((reachable-apples (filter (lambda (apple) 
                                     (let ((height (car apple)))
                                       (and (>= height chair-height)
                                            (<= height (+ chair-height tao-max-reach)))))
                                   apple-data))
         (sorted-apples (sort reachable-apples (lambda (a b) (< (cadr a) (cadr b))))))
    (let loop ((apples sorted-apples)
               (energy init-energy)
               (count 0))
      (if (or (null? apples) (< energy 0))
          count
          (let ((current-apple (car apples))
                (remaining-energy (- energy (cadr (car apples)))))
            (if (>= remaining-energy 0)
                (loop (cdr apples) remaining-energy (+ count 1))
                count))))))


(define (test max-apples-collected)
(check-equal? (max-apples-collected '((100 4) (150 3) (120 5)) 20 130 10) 2)
(check-equal? (max-apples-collected '((110 7) (90 3) (200 6)) 15 125 8) 1)
(check-equal? (max-apples-collected '((180 2) (130 1) (170 4)) 25 140 15) 1)
(check-equal? (max-apples-collected '((160 3) (140 5) (110 2)) 30 135 12) 3)
(check-equal? (max-apples-collected '((120 1) (130 2) (140 3)) 15 120 5) 2))

(test max-apples-collected)