#lang racket

(require racket/gui/base)
(require racket/draw)

(require "board-org.rkt")
(require "util.rkt")

(define *BOARD-MARGIN* 25)
(define *CELL-WIDTH* 50) 

(define *main-frame*
        (new frame%
        [label "I'm a SUDOKU!"]
        [width (+ (* 9 *CELL-WIDTH*) (* 2 *BOARD-MARGIN*))]
        [height (+ (* 10 *CELL-WIDTH*) (* 2 *BOARD-MARGIN*))]))

(define (get-mouse-coord event)
  (let ([x (send event get-x)]
        [y (send event get-y)])
    (values x y)))

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (cond
        [(send event get-left-down)
         (let-values ([(x y) (get-mouse-coord event)])
           (send msg set-label (format "(x,y) -> ~a,~a" x y)))]
        [(send event get-right-down)
         (send msg set-label "RIGHT-DOWN")]
        [else (send msg set-label "No mouse")]))
    
    ; Define overriding method to handle keyboard events
    (define/override (on-char key-event)
      (cond
        [(equal? (send key-event get-key-code) #\a)
         (send msg set-label "A")]
        [(equal? (send key-event get-key-code) 'down)
         (send msg set-label "DOWN")]
        [else (send msg set-label "Other")]))
    
    ; Call the superclass init, passing on all init args
    (super-new)))

(new my-canvas%
     [parent *main-frame*]
     [paint-callback
      (λ (canvas dc)
         (send dc set-smoothing 'smoothed)
         (send dc set-text-foreground "blue")
         (send dc set-pen "black" 1 'solid)
         (send dc set-brush "red" 'solid)
         (draw-board dc sudoku-board))])

; Make a static text message in the frame
(define msg (new message% [parent *main-frame*]
                          [stretchable-height #f]
                          [label "No events so far for mouse..."]))

(define (draw-grid dc)
        (let ([offset 0]
              [dbl-set (set 0 3 6 9)])
          (for ([i 10])
            (if (set-member? dbl-set i) 
              (send dc set-pen "black" 3 'solid)
              (send dc set-pen "black" 1 'solid))
            (set! offset (+ *BOARD-MARGIN* (* i *CELL-WIDTH*)))
            (send dc draw-line *BOARD-MARGIN* offset 475 offset)
            (send dc draw-line offset *BOARD-MARGIN* offset 475))))

(define (offset-in-grid row col)
        (values (* (- col 1) *CELL-WIDTH*) (* (- row 1) *CELL-WIDTH*)))    

(define (draw-number dc row col n)
  (let-values ([(x y) (offset-in-grid row col)])
     (send dc draw-text (string n) (+ x 45) (+ y 40))))

(define (get-value sudoku-board row col)
  (vector-ref sudoku-board (+ (* (sub1 row) 9) (sub1 col))))

(define (draw-board-numbers dc sudoku-board)
  (let ([c #\0])
  (for ([row (in-range 1 10)])
    (for ([col (in-range 1 10)])
      (draw-number dc row col (vector-ref sudoku-board (+ (* (sub1 row) 9) (sub1 col))))))))

(define (draw-board dc sudoku-board)
  (draw-grid dc)
  (draw-board-numbers dc sudoku-board))

(send *main-frame* show #t)
