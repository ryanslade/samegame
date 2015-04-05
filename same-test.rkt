#lang racket

(require rackunit
         "same.rkt")

(define empty-row (list 0 0 0 0 0))
(define eb (make-empty-board 10 10))

(check-equal? '(1 0 0 0 0)
             (set-x empty-row 0 1))

(check-equal? '(0 0 0 0 1)
             (set-x empty-row 4 1))

(check-equal? '(0 0 0 0 0)
             (set-x empty-row 0 0))

; neighbors
(check-equal? 4 (length (neighbors eb (pos 3 3))))
(check-equal? 2 (length (neighbors eb (pos 0 0))))
(check-equal? 2 (length (neighbors eb (pos 9 9))))

; valid-pos?
(check-true (valid-pos? eb (pos 0 0)))


