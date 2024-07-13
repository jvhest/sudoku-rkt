#lang racket
;; File: sudoku-org.rkt
(provide show-frame)

(require racket/gui/base)
(require racket/draw)

(require "sudoku-world.rkt")
(require "board.rkt")
(require "../tests/test-data.rkt")
(require "util.rkt")

(define BOARD-MARGIN 25)
(define CELL-WIDTH 75)
(define MAX-BORDER (+ BOARD-MARGIN (* CELL-WIDTH 9)))

(define FONT-W 27)
(define FONT-H 63)
(define X-OFFSET (round (/ (- CELL-WIDTH FONT-W) 2)))
(define Y-OFFSET (round (/ (- CELL-WIDTH FONT-H) 2)))

(define font-numbers (make-font
                      #:size 45
                      #:family 'modern
                      #:weight 'bold
                      #:size-in-pixels? #t))

(define *main-frame*
        (new frame%
        [label "I'm a SUDOKU!"]
        [width (+ (* 9 CELL-WIDTH) (* 2 BOARD-MARGIN))]
        [height (+ (* 10 CELL-WIDTH) (* 2 BOARD-MARGIN))]))

(define (draw-grid dc)
        (let ([offset 0]
              [dbl-set (set 0 3 6 9)])
          (for ([i 10])
            (if (set-member? dbl-set i) 
              (send dc set-pen "black" 3 'solid)
              (send dc set-pen "black" 1 'solid))
            (set! offset (+ BOARD-MARGIN (* i CELL-WIDTH)))
            (send dc draw-line BOARD-MARGIN offset MAX-BORDER offset)
            (send dc draw-line offset BOARD-MARGIN offset MAX-BORDER))))

; Make a static text message in the frame
(define msg (new message% [parent *main-frame*]
                          [stretchable-height #f]
                          [label "No events so far for mouse..."]))

; Derive a new canvas (a drawing window) class to handle events
(define sudoku-canvas%
  (class canvas%
    (inherit
      get-width
      get-height
      refresh)

    (field
     [cursor-col 0]
     [cursor-row 0])

    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (cond
        [(send event get-left-down)
        (let ([x (send event get-x)]
              [y (send event get-y)])
          (when (not (or
                 (< x BOARD-MARGIN) (< y BOARD-MARGIN)
                 (> x (+ (* CELL-WIDTH 9) BOARD-MARGIN))
                 (> y (+ (* CELL-WIDTH 9) BOARD-MARGIN))))
            (set! cursor-col (add1 (quotient (- x BOARD-MARGIN) CELL-WIDTH)))
            (set! cursor-row (add1 (quotient (- y BOARD-MARGIN) CELL-WIDTH)))
            (send *main-frame* refresh)
            (send msg set-label
                  (format "(x,y) -> ~a,~a | (r,c) -> ~a,~a" x y cursor-row cursor-col))))]))

    ; Define overriding method to handle keyboard events
    (define/override (on-char key-event)
      (when (equal? (send key-event get-key-release-code) 'press)
        (let ([n (send key-event get-key-code)])
          (cond
            [(member n (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
             (begin
               (set-value CURR-SUDOKU cursor-row cursor-col (- (char->integer n) 48))
               (send *main-frame* refresh))]
            [(equal? n #\s)
             (if (solve-game CURR-SUDOKU)
                 (send *main-frame* refresh)
                 (send msg set-label "Sudoku has no solution!!"))]))))

    ; Call the superclass init, passing on all init args
    (super-new)))

(define cv
  (new sudoku-canvas%
       [parent *main-frame*]
       [paint-callback
        (λ (canvas dc)
          (send dc set-smoothing 'smoothed)
          (send dc set-text-foreground "blue")
          (send dc set-pen "black" 1 'solid)
          (send dc set-brush "light gray" 'solid)
          (send dc set-font font-numbers)
          (draw-board dc CURR-SUDOKU))]))

;; offset in grid relative to top left corner of sudoku-grid
(define (offset-in-grid row col)
  (values (+ (* (sub1 col) CELL-WIDTH) BOARD-MARGIN X-OFFSET)
          (+ (* (sub1 row) CELL-WIDTH) BOARD-MARGIN Y-OFFSET)))

;; offset in grid relative to top left corner of sudoku-grid
(define (top-left-xy-of-cell row col)
  (values (+ (* (sub1 col) CELL-WIDTH) BOARD-MARGIN)
          (+ (* (sub1 row) CELL-WIDTH) BOARD-MARGIN)))

(define (draw-board-numbers dc sudoku)
  (let ([cursor-r (get-field cursor-row cv)]
        [cursor-c (get-field cursor-col cv)])
  (for ([row (in-range 1 10)])
    (for ([col (in-range 1 10)])
      (when (and (= row cursor-r) (= col cursor-c))
        (let-values ([(x y) (top-left-xy-of-cell row col)])
        ;; (send dc set-brush "red" 'solid)
        (send dc draw-rectangle x y CELL-WIDTH CELL-WIDTH)))
      (let ([n (get-value sudoku row col)])
        (unless (= n 0)
          (let-values ([(x y) (offset-in-grid row col)])
            (send dc draw-text (~a n) x y))))))))

(define (draw-board dc sudoku)
  (draw-grid dc)
  (draw-board-numbers dc sudoku))

(define (show-frame)
  (send *main-frame* show #t))

(show-frame)

(define (tesje letter)
  (define text-size-dc (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))
  (send text-size-dc set-font font-numbers)
  (send text-size-dc get-text-extent letter))
