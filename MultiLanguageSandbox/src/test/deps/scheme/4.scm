#lang racket
(require rackunit)

(define (longest-common-prefix strs)
  (if (null? strs)
      ""
      (let ((first-str (car strs)))
        (let loop ((i 0) (prefix ""))
          (if (>= i (string-length first-str))
              prefix
              (let ((current-char (string-ref first-str i)))
                (if (all-char-equal? current-char strs i)
                    (loop (+ i 1) (string-append prefix (string current-char)))
                    prefix)))))))

(define (all-char-equal? char strs index)
  (if (null? strs)
      #t
      (let ((str (car strs)))
        (and (< index (string-length str))
             (char=? char (string-ref str index))
             (all-char-equal? char (cdr strs) index)))))


(define (check longest-common-prefix)
(check-equal? (longest-common-prefix '("flower" "flow" "flight")) "fl")
(check-equal? (longest-common-prefix '("dog" "racecar" "car")) "")
(check-equal? (longest-common-prefix '("interspecies" "interstellar" "interstate")) "inters")
(check-equal? (longest-common-prefix '("hello")) "hello")
(check-equal? (longest-common-prefix '()) "")
(check-equal? (longest-common-prefix '("prefix" "precalculate" "precarious" "presume")) "pre"))

(check longest-common-prefix)