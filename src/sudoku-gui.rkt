#lang racket
;; File: sudoku-gui.rkt

(provide sudoku-gui%)

(require racket/gui/base)
(require racket/draw)

(require "sudoku.rkt")
(require "util.rkt")

(define sudoku-gui%
  (class object%
    (init-field
     sudoku)

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

    (define main-frame (new frame%
                            [label "I'm a SUDOKU!"]
                            [width (+ (* 9 CELL-WIDTH) (* 2 BOARD-MARGIN))]
                            [height (+ (* 10 CELL-WIDTH) (* 2 BOARD-MARGIN))]))

    (define/public (show-frame)
      (send main-frame show #t))


    ;; Private
    (super-new)
    
    ; Make a static text message in the frame
    (define msg (new message%
                     [parent main-frame]
                     [stretchable-height #f]
                     [label "No events so far for mouse..."]))

    ; Derive a new canvas (a drawing window) class to handle events
    (define sudoku-canvas%
      (class canvas%
        (inherit
          get-width
          get-height
          refresh)

        ; Define overriding method to handle mouse events
        (define/override (on-event event)
          (cond
            [(send event get-left-down)
             (let-values ([(x y) (values (send event get-x) (send event get-y))])
               (when (not (or
                           (< x BOARD-MARGIN) (< y BOARD-MARGIN)
                           (> x (+ (* CELL-WIDTH 9) BOARD-MARGIN))
                           (> y (+ (* CELL-WIDTH 9) BOARD-MARGIN))))
                 (let-values ([(row col) (pixel-to-cursor x y)])
                   (send sudoku set-cursor row col)
                   (send main-frame refresh)
                   (send msg set-label
                         (format "(x,y) -> ~a,~a | (r,c) -> ~a,~a" x y row col)))))]))

        ; Define overriding method to handle keyboard events
        (define/override (on-char key-event)
          (when (equal? (send key-event get-key-release-code) 'press)
            (let ([n (send key-event get-key-code)])
;;               (display "key ~a\n" n)
              (cond
                [(member n (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                 (begin
                   (send sudoku set-cursor-val (- (char->integer n) 48))
                   (send main-frame refresh))]
                [(equal? n #\?)
                 (begin
                   (send sudoku set-cursor-val (send sudoku get-cursor-solved))
                   (send main-frame refresh))]
                [(equal? n #\!)
                 (begin
                   (set-field! easy sudoku (not (get-field easy sudoku)))
                   (send main-frame refresh))]
                [(member n (list 'up #\k 'down #\j 'left #\h 'right #\l))
                 (when (send sudoku move-cursor n)
                   (send main-frame refresh))]
                [(equal? n #\s)
                 (begin
                   (send sudoku show-solved)
                   (send sudoku print-puzzle)
                   (send main-frame refresh))]))))

        ; Call the superclass init, passing on all init args
        (super-new)

        (define/private (pixel-to-cursor x y)
          (values 
           (add1 (quotient (- y BOARD-MARGIN) CELL-WIDTH))   ; row
           (add1 (quotient (- x BOARD-MARGIN) CELL-WIDTH)))) ; column

        ))

    (new sudoku-canvas%
         [parent main-frame]
         [paint-callback
          (λ (canvas dc)
            (send dc set-smoothing 'smoothed)
            (send dc set-text-foreground "blue")
            (send dc set-pen "black" 1 'solid)
            (send dc set-brush "light gray" 'solid)
            (send dc set-font font-numbers)
            (draw-board-numbers dc)
            (draw-grid dc))])

    (define/private (draw-grid dc)
      (let ([offset 0]
            [dbl-set (set 0 3 6 9)])
        (for ([i 10])
          (if (set-member? dbl-set i)
              (send dc set-pen "black" 3 'solid)
              (send dc set-pen "black" 1 'solid))
          (set! offset (+ BOARD-MARGIN (* i CELL-WIDTH)))
          (send dc draw-line BOARD-MARGIN offset MAX-BORDER offset)
          (send dc draw-line offset BOARD-MARGIN offset MAX-BORDER))))

    ;; offset in grid relative to top left corner of sudoku-grid
    (define/private (offset-in-grid row col)
      (values (+ (* (sub1 col) CELL-WIDTH) BOARD-MARGIN X-OFFSET)
              (+ (* (sub1 row) CELL-WIDTH) BOARD-MARGIN Y-OFFSET)))

    ;; offset in grid relative to top left corner of sudoku-grid
    (define/private (top-left-xy-of-cell row col)
      (values (+ (* (sub1 col) CELL-WIDTH) BOARD-MARGIN)
              (+ (* (sub1 row) CELL-WIDTH) BOARD-MARGIN)))

    (define/private (draw-board-numbers dc)
      (send dc set-pen "black" 1 'solid)
      (let-values ([(cursor-r cursor-c) (send sudoku get-cursor)])
        
        (define (draw-cell dc row col bg fg digit)
          (let-values ([(x y) (top-left-xy-of-cell row col)])
            (if (and (= row cursor-r) (= col cursor-c))
                (send dc set-brush "gray" 'solid)
                (send dc set-brush bg 'solid))
            (send dc draw-rectangle x y CELL-WIDTH CELL-WIDTH)
            (when (> digit 0)
              (send dc set-text-foreground fg)
              (send dc draw-text (~a digit) (+ x X-OFFSET) (+ y Y-OFFSET)))
            ))
        
        (for ([row (in-range 1 10)])
          (for ([col (in-range 1 10)])
            ;; write digits
            (let ([digit (send sudoku get-value row col)])
              (cond
                  [(send sudoku is-static? row col)
                   (draw-cell dc row col "white smoke" "black" digit)]
                  [(and (not (equal? digit 0))
                        (get-field easy sudoku)
                        (not (equal? digit (send sudoku get-value-solved row col))))
                   (draw-cell dc row col "red" "yellow" digit)]
                  [else
                   (draw-cell dc row col "white" "blue" digit)]))))))

    (define/private (letter-test letter)
      (define text-size-dc (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))
      (send text-size-dc set-font font-numbers)
      (send text-size-dc get-text-extent letter))

    ))
