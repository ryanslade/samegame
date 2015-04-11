#lang racket

(require htdp/world)

(define board-width 20)
(define board-height 20)
(define window-width 600)
(define window-height 600)
(define piece-radius (/ window-width board-width 2))
(define tick-seconds (/ 1 28))

; 0 is blank
(define piece-colors
  (hash 0 'white
        1 'red
        2 'blue
        3 'orange
        ))

(define num-colors (sub1 (hash-count piece-colors)))
(define (random-color) (add1 (random num-colors)))

(struct pos (x y) #:transparent)

; Make a board of all zeros
(define (make-empty-board width height)
  (build-list height (lambda (n) (make-list width 0))))

; Set the position at x to the val returning a new list
(define (set-x lst x val)
  (append (take lst x) (list val) (drop lst (add1 x))))

; Set the position at x,y to value
; Top left is considered 0, 0
(define (set-pos board pos val)
  ; new-board is the new board so far
  ; yy is our current y position through the old board
  (let loop ([new-board empty] [yy 0])
    (cond
      ; Done
      [(= yy (length board)) new-board]
      ; The row we need to modify
      [(= yy (pos-y pos)) 
       (let ([old-row (list-ref board yy)]) 
         (loop (append new-board (list (set-x old-row (pos-x pos) val))) (add1 yy)))]
      ; Any other row
      [else (loop (append new-board (list (list-ref board yy))) (add1 yy))])))

; Get the value at the board position p
(define (get-value board p)
  (list-ref (list-ref board (pos-y p)) (pos-x p)))

; Checks that a position is within the bounds of the board
(define (valid-pos? board p)
  (let ([x (pos-x p)]
        [y (pos-y p)])
    (and (not (negative? x)) 
         (not (negative? y))
         (< y (length board))
         (< x (length (list-ref board 0))))))

(define (get-column board column)
  (map (lambda (x) (list-ref x column)) board))

; Remove zeros from a column and place them at the end.
(define (collapse-column column)
  (let-values ([(z nz) (partition zero? column)]) (append z nz)))

; Swaps rows with columns
(define (swap-rows-columns board)
  (map (lambda (n) (get-column board n)) (range (length board))))

(define (collapse-board board) 
  (swap-rows-columns 
   (map collapse-column ; Rename this? 
        (swap-rows-columns board))))

; Shift pieces to the right of an empty column left
(define (shift-empty-columns board) board
  (swap-rows-columns
   (let-values 
       ([(z nz) (partition (lambda (r) (andmap zero? r)) (swap-rows-columns board))]) 
     (append nz z)))) 

; Get positions of neighbors
(define (neighbors board p)
  (let ([x (pos-x p)]
        [y (pos-y p)])            
    (filter (lambda (p) (valid-pos? board p)) 
            (list 
             (pos (add1 x) y)
             (pos (sub1 x) y)
             (pos x (add1 y))
             (pos x (sub1 y))))))

(define (color-neighbors board p)
  (let ([value (get-value board p)])
    (filter (lambda (x) (= value (get-value board x))) (neighbors board p))))

(define (color-neighbors-and-self-set board p)
  (list->set (append (list p) (color-neighbors board p))))

; Given a position, get pieces connected that are the same colour
(define (connected board p)
  (let ([value (get-value board p)])
    (let loop ([seen (color-neighbors-and-self-set board p)])      
      (let ([next 
             (apply set-union 
                    (set-map seen 
                             (lambda (p) (color-neighbors-and-self-set board p))))])
        (if (set=? seen next) next
            (loop next))))))

; Get a pos by index into the board.
; 0  -> (pos 0 0)
; 10 -> (pos 0 1)
; e
(define (count-to-pos count)
  (pos (modulo count board-width) (quotient count board-height)))

; create a new board, randomly populated
(define (new-board) 
  (let loop ([board (make-empty-board board-width board-height)]
             [count 0])
    (if (= count (* board-width board-height)) board
        (loop (set-pos board (count-to-pos count) (random-color)) (add1 count)))))

(define (pos-from-xy x y)
  (pos 
   (quotient x (* 2 piece-radius)) 
   (quotient y (* 2 piece-radius))))

; value is a number mapping to a color in the
; colors hash
(define (board-piece value) 
  (circle piece-radius 'solid (hash-ref piece-colors value)))

; Given a board position, either x or y, return the pixel position
(define (pixel-pos board-pos)
  (+ piece-radius (* board-pos (* 2 piece-radius))))

(define (render world)
  (let loop ([scene (empty-scene window-width window-height)] [count 0])
    (if (= count (* board-width board-height)) scene
        (loop
         (let ([p (count-to-pos count)])
           (place-image (board-piece (get-value world (count-to-pos count)))
                        (pixel-pos (pos-x p))
                        (pixel-pos (pos-y p)) 
                        scene)) 
         (add1 count)))))


(define (handle-click board x y ev)
  (if (symbol=? ev 'button-up)
      (if (= 1 (set-count (connected board (pos-from-xy x y)))) 
          board
          (let loop ([pieces (connected board (pos-from-xy x y))] [board board])
            (if (set-empty? pieces) 
                (shift-empty-columns (collapse-board board))
                (loop (set-rest pieces) (set-pos board (set-first pieces) 0)))))
      board))

(define (start-game) 
  (big-bang window-width window-height tick-seconds (new-board))
  (on-redraw render)
  (on-mouse-event handle-click))
