#lang racket

(require rackunit
         "same.rkt")

(require/expose "same.rkt" 
                (make-empty-board
                 list-update
                 neighbors
                 valid-pos?
                 get-column
                 collapse-column
                 swap-rows-columns
                 collapse-board
                 shift-empty-columns
                 pos))

(define test-board '((11 21 31 41 51)
                     (12 22 32 42 52)
                     (13 23 33 43 53)
                     (14 24 34 44 54)
                     (15 25 35 45 55)))

(define empty-row (list 0 0 0 0 0))
(define eb (make-empty-board 10 10))

(check-equal? (list-update empty-row 0 1)
              '(1 0 0 0 0))

(check-equal? (list-update empty-row 4 1) 
              '(0 0 0 0 1))

(check-equal? (list-update empty-row 0 0)
              '(0 0 0 0 0))

; neighbors
(test-case
 "neighbors"
 (check-equal? (length (neighbors eb (pos 3 3))) 4)
 (check-equal? (length (neighbors eb (pos 0 0))) 2)
 (check-equal? (length (neighbors eb (pos 9 9))) 2))

(test-case
 "valid-pos?"
 (check-true (valid-pos? eb (pos 0 0)))
 (check-false (valid-pos? eb (pos -1 0)))
 (check-false (valid-pos? eb (pos 0 -1)))
 (check-true (valid-pos? eb (pos 1 1))))

(check-equal? (get-column test-board 0) '(11 12 13 14 15))

(test-case 
 "collapse-column"
 (check-equal? (collapse-column '(0 1 2 3 4)) '(0 1 2 3 4))
 (check-equal? (collapse-column '(1 2 3 4 5)) '(1 2 3 4 5))
 (check-equal? (collapse-column '(1 0 3 0 5)) '(0 0 1 3 5))
 (check-equal? (collapse-column '(0 0 0 0 0)) '(0 0 0 0 0)))

(check-equal? (swap-rows-columns  '((11 12 13 14 15) 
                                    (21 22 23 24 25)
                                    (31 32 33 34 35)
                                    (41 42 43 44 45)
                                    (51 52 53 54 55))) test-board)

(check-equal? (swap-rows-columns (swap-rows-columns test-board)) test-board)

(define collapsible-board '((11 21 31 41 0)
                            (12 22 32 0  52)
                            (0  23 33 43 53)
                            (14 0  34 0  54)
                            (15 0  35 45 55)))

(define collapsed-board   '((0  0  31 0  0)
                            (11 0  32 0  52)
                            (12 21 33 41 53)
                            (14 22 34 43 54)
                            (15 23 35 45 55)))

(check-equal? (collapse-board collapsible-board) collapsed-board)

(check-equal? (shift-empty-columns 
               '((1 1 1 0 1)
                 (1 1 1 0 1)
                 (1 1 1 0 1)
                 (1 1 1 0 1)
                 (1 1 1 0 1)))
              
              '((1 1 1 1 0)
                (1 1 1 1 0)
                (1 1 1 1 0)
                (1 1 1 1 0)
                (1 1 1 1 0)))


