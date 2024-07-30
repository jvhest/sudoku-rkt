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
    (define CELL-WIDTH 85)
    (define MAX-BORDER (* CELL-WIDTH 9))
    (define FONT-W 27)
    (define FONT-H 63)
    (define X-OFFSET (round (/ (- CELL-WIDTH FONT-W) 2)))
    (define Y-OFFSET (round (/ (- CELL-WIDTH FONT-H) 2)))

    ;; activate warning for invalid moves
    (define warnings #f)

    (define cursor-r 5)
    (define cursor-c 5)

    (define/private (toggle-warnings)
      (set! warnings (not warnings)))

    (define/private (show-warnings?) warnings)

    (define font-numbers (make-font
                          #:size 45
                          #:family 'modern
                          #:weight 'bold
                          #:size-in-pixels? #t))

    (define font-buttons (make-font
                          #:size 20
                          #:family 'modern
                          #:weight 'semibold
                          #:size-in-pixels? #t))
    
    (define main-frame (new frame%
                            [label "I'm a SUDOKU!"]))

    (define/public (start-gui)
      (send main-frame show #t))

    (super-new)

    ;; direction oneof 'up 'down 'left 'right -> boolean (#t need refresh)
    (define/private (move-cursor key)
      (cond
        [(and (equal? key 'up) (> cursor-r 1))
         (set!-values (cursor-r cursor-c) (values (sub1 cursor-r) cursor-c))]
        [(and (equal? key 'down) (< cursor-r 9))
         (set!-values (cursor-r cursor-c) (values (add1 cursor-r) cursor-c))]
        [(and (equal? key 'left) (> cursor-c 1))
         (set!-values (cursor-r cursor-c) (values cursor-r (sub1 cursor-c)))]
        [(and (equal? key 'right) (< cursor-c 9))
         (set!-values (cursor-r cursor-c) (values cursor-r (add1 cursor-c)))]
        [else #f]))  ; no refresh needed

    ; Make a static text message in the frame
    (define msg (new message%
                     [parent main-frame]
                     [min-height 30]
                     [vert-margin 10]
                     [font font-buttons]
                     [label "No events so far for mouse..."]))

    ; Derive a new canvas (a drawing window) class to handle events
    (define sudoku-canvas%
      (class canvas%
        (inherit
         ;; get-width
         ;; get-height
         refresh)

        ; Define overriding method to handle mouse events
        (define/override (on-event event)
          (cond
            [(send event get-left-down)
             (let-values ([(x y) (values (send event get-x) (send event get-y))])
               (let-values ([(row col) (pixel-to-cursor x y)])
                 (set!-values (cursor-r cursor-c) (values row col))
                 (send main-frame refresh)
                 (send msg set-label
                       (format "(x,y) -> ~a,~a | (r,c) -> ~a,~a" x y row col))))]))

        ; Define overriding method to handle keyboard events
        (define/override (on-char key-event)
          (when (equal? (send key-event get-key-release-code) 'press)
            (let ([n (send key-event get-key-code)])
              (cond
                [(member n (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                 (begin
                   (send sudoku set-value cursor-r cursor-c n)
                   (send main-frame refresh))]
                ;; give hint for current cell
                [(equal? n #\?)
                 (begin
                   (send sudoku set-value cursor-r cursor-c
                         (send sudoku get-value cursor-r cursor-c #:mode 'solved))
                   (send main-frame refresh))]
                [(member n (list 'up 'down 'left 'right))
                 (when (move-cursor n)
                   (send main-frame refresh))]
                ;; show sudoku solution
                [(equal? n #\s)
                 (begin
                   (send sudoku show-solved)
                   (send sudoku print-puzzle)
                   (send main-frame refresh))]))))

        ; Call the superclass init, passing on all init args
        (super-new)

        (define/private (pixel-to-cursor x y)
          (values 
           (add1 (quotient y CELL-WIDTH))   ; row
           (add1 (quotient x CELL-WIDTH)))) ; column
        ))

    (new sudoku-canvas%
         [parent main-frame]
         [min-width (* 9 CELL-WIDTH)]
         [min-height (* 9 CELL-WIDTH)]
         [vert-margin BOARD-MARGIN]
         [horiz-margin BOARD-MARGIN]
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
          (set! offset (* i CELL-WIDTH))
          (send dc draw-line 0 offset MAX-BORDER offset)
          (send dc draw-line offset 0 offset MAX-BORDER))))

    ;; offset in grid relative to top left corner of sudoku-grid
    (define/private (top-left-xy-of-cell row col)
      (values (* (sub1 col) CELL-WIDTH)
              (* (sub1 row) CELL-WIDTH)))

    (define/private (draw-board-numbers dc)
      (send dc set-pen "black" 1 'solid)

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
              [(and warnings (send sudoku is-invalid? digit row col))
               (draw-cell dc row col "red" "yellow" digit)]
              [else
               (draw-cell dc row col "white" "blue" digit)])))))

    (define panel
      (new horizontal-panel%
           [parent main-frame]
           [alignment '(left center)]))

    (define (new-game)
      (send sudoku new-puzzle)
      (set!-values (cursor-r cursor-c) (values 5 5))
      (send main-frame refresh))
    
    (new button% [parent panel]
         [label "New"]
         [min-width 150]
         [min-height 50]
         [vert-margin 25]
         [horiz-margin 12]
         [font font-buttons]
         [callback (lambda (button event) (new-game))])

    (define (save-game)
        (send sudoku save-puzzle)
        (send main-frame refresh))
    
    (new button% [parent panel]
         [label "Save"]
         [min-width 150]
         [min-height 50]
         [vert-margin 25]
         [horiz-margin 12]
         [font font-buttons]
         [callback (lambda (button event) (save-game))])

    (new check-box% [parent panel]
         [label "Feedback"]	 
         [min-width 200]
         [min-height 50]
         [vert-margin 25]
         [horiz-margin 12]
         [font font-buttons]
         [callback (lambda (c e)  ;; toggle warnings field
                     (set! warnings (not warnings))
                     (send main-frame refresh))])

    (define/private (letter-test letter)
      (define text-size-dc (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))
      (send text-size-dc set-font font-numbers)
      (send text-size-dc get-text-extent letter))

    ))
