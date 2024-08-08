#lang racket
;; File: sudoku-gui.rkt

(provide sudoku-gui%)

(require racket/gui/base)
(require racket/draw)

(require "core.rkt")
(require "nyt.rkt")
(require "util.rkt")

(define sudoku-gui%
  (class object%

    (define BOARD-MARGIN 25)
    (define CELL-WIDTH 95)
    (define MAX-BORDER (* CELL-WIDTH 9))
    (define FONT-W 27)
    (define FONT-H 63)
    (define X-OFFSET (round (/ (- CELL-WIDTH FONT-W) 2)))
    (define Y-OFFSET (round (/ (- CELL-WIDTH FONT-H) 2)))

    ;; activate warning for invalid moves
    (define warnings #f)
    (define show-options #f)
    (define sudoku-name "")
    (define data-mode 'play)

    (define cursor-cell (get-cell 5 5))
    (define (cursor-row) (cell-y cursor-cell))
    (define (cursor-col) (cell-x cursor-cell))

    (define/private (toggle-warnings)
      (set! warnings (not warnings)))

    (define/private (show-warnings?) warnings)

    (define font-numbers (make-font
                          #:size 45
                          #:family 'modern
                          #:weight 'bold
                          #:size-in-pixels? #t))

    (define font-message (make-font
                          #:size 20
                          #:family 'modern
                          #:weight 'semibold
                          #:size-in-pixels? #t))

    (define font-options (make-font
                          #:size 14
                          #:family 'modern
                          #:weight 'semilight
                          #:size-in-pixels? #t))

    (define main-frame (new frame%
                            [label "I am Sudoku"]))

    (define menu-bar
      (new menu-bar% [parent main-frame]))

    (define file-menu (new menu% [parent menu-bar]
         [label "&File"]))

    (new menu-item% [parent file-menu]
         [label "&New Sudoku"]
         [callback (lambda (c e)
                     (set! data-mode 'play)
                     (load-puzzle)
                     (set! cursor-cell (get-cell 5 5))
                     (send msg set-color #f)
                     (send msg set-label
                           (format "Sudoku: ~a" curr-file))
                     (send main-frame refresh)
                     (send my-canvas focus))])

    (new menu-item% [parent file-menu]
         [label "&Save Sudoku"]
         [callback (lambda (c e)
                     (save-puzzle)
                     (send msg set-label
                           (format "Sudoku: ~a" curr-file))
                     (send main-frame refresh)
                     (send my-canvas focus))])

    (new menu-item% [parent file-menu]
         [label "&Download NYT Sudoku"]
         [callback (lambda (c e)
                     (get-nyt-sudoku-puzzles)
                     (send main-frame refresh)
                     (send my-canvas focus))])

    (new menu-item% [parent file-menu]
         [label "&Quit"]
         [callback (lambda (c e)
                     (send main-frame show #f))])

    (define preference-menu (new menu% [parent menu-bar]
         [label "&Preference"]))

    (new checkable-menu-item% [parent preference-menu]
         [label "&Feedback"]
         [callback (lambda (c e)  ;; toggle warnings field
                     (set! warnings (not warnings))
                     (send main-frame refresh)
                     (send my-canvas focus))])

    (new checkable-menu-item% [parent preference-menu]
         [label "&Show Options"]
         [callback (lambda (c e)  ;; toggle show-options field
                     (set! show-options (not show-options))
                     (send main-frame refresh)
                     (send my-canvas focus))])

    (define help-menu (new menu% [parent menu-bar]
         (label "&Help")))

    (define top-panel (new horizontal-panel% [parent main-frame]
                           [alignment '(center center)]))

    ; Make a static text message in the frame
    (define msg (new message%
                     [parent top-panel]
                     [min-width (* 5 CELL-WIDTH)]
                     [font font-message]
                     [label "No Sudoku loaded!"]))

    (define/public (start-gui)
      (send main-frame show #t)
      (send my-canvas focus))

    (super-new)

    ;; direction one of 'up 'down 'left 'right -> boolean (#t need refresh)
    (define/private (move-cursor key)
      (cond
        [(and (equal? key 'up) (> (cursor-row) 1))
         (set! cursor-cell (get-cell (sub1 (cursor-row)) (cursor-col)))]

        [(and (equal? key 'down) (< (cursor-row) 9))
         (set! cursor-cell (get-cell (add1 (cursor-row)) (cursor-col)))]

        [(and (equal? key 'left) (> (cursor-col) 1))
         (set! cursor-cell (get-cell (cursor-row) (sub1 (cursor-col))))]

        [(and (equal? key 'right) (< (cursor-col) 9))
         (set! cursor-cell (get-cell (cursor-row) (add1 (cursor-col))))]
        [else #f]))  ; no refresh needed

    (define (sudoku-complete-msg)
      (send msg set-color "medium blue")
      (send msg set-label
            (format "SUDOKU FINISHED (with ~a errors)" (solved-with-errors))))

    ; Derive a new canvas (a drawing window) class to handle events
    (define sudoku-canvas%
      (class canvas%
        (inherit
         refresh)

        ; Define overriding method to handle mouse events
        (define/override (on-event event)
          (cond
            [(send event get-left-down)
             (let-values
                 ([(x y) (values (send event get-x) (send event get-y))])
               (let-values ([(row col) (pixel-to-cursor x y)])
                 (set! cursor-cell (get-cell row col))
                 (send main-frame refresh)))]))

        ; Define overriding method to handle keyboard events
        (define/override (on-char key-event)
          (when (equal? (send key-event get-key-release-code) 'press)
            (let ([n (send key-event get-key-code)])
              (cond
                [(member n (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
                 (begin
                   (set-cell-play! cursor-cell (char->digit n))
                   (set-cell-invalid?!
                    cursor-cell
                    (and (not (= (cell-play cursor-cell) 0))
                         (not (= (cell-play cursor-cell)
                                 (cell-solved cursor-cell)))))
                   (update-options-for-cells)
                   (when (solved?)
                     (sudoku-complete-msg))
                   (send main-frame refresh))]
                ;; give hint for current cell
                [(equal? n #\?)
                 (begin
                   (set-cell-play! cursor-cell (cell-solved cursor-cell))
                   (update-options-for-cells)
                   (when (solved?)
                     (sudoku-complete-msg))
                   (send main-frame refresh))]
                [(member n (list 'up 'down 'left 'right))
                 (when (move-cursor n)
                   (send main-frame refresh))]
                ;; show sudoku solution
                [(equal? n #\s)
                 (if (equal? data-mode 'play)
                     (set! data-mode 'solved)
                     (set! data-mode 'play))
                 (send main-frame refresh)]))))

        ; Call the superclass init, passing on all init args
        (super-new)

        (define/private (pixel-to-cursor x y)
          (values
           (add1 (quotient y CELL-WIDTH))   ; row
           (add1 (quotient x CELL-WIDTH)))) ; column
        ))

(define my-canvas
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
            (draw-board-numbers dc #:mode data-mode)
            (draw-grid dc))]))

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
    (define (top-left-xy-of-cell c)
      (values (* (sub1 (cell-x c)) CELL-WIDTH)
              (* (sub1 (cell-y c)) CELL-WIDTH)))

    (define (draw-board-numbers dc #:mode mode)
      (send dc set-pen "black" 1 'solid)

      (define (draw-cell c)
        (let-values ([(x y) (top-left-xy-of-cell c)])

          (define (is-cursor?)
            (and (= (cell-y c) (cell-y cursor-cell))
                 (= (cell-x c) (cell-x cursor-cell))))

          (define (fill-bg-cell bg)
              (if (is-cursor?)
                  (send dc set-brush "gray" 'solid)
                  (send dc set-brush bg 'solid))
              (send dc draw-rectangle x y CELL-WIDTH CELL-WIDTH))

          (define (write-digit fg)
              (let ([digit (if (equal? mode 'solved)
                               (cell-solved c)
                               (cell-play c))])
                (send dc set-text-foreground fg)
                (send dc draw-text (~a digit) (+ x X-OFFSET) (+ y Y-OFFSET))))

          (define (write-options)
            (when (equal? show-options #t)
              (send dc set-font font-options)
              (let ([row 10])
                (for-each
                 (lambda (l3)
                   (send dc draw-text (~a l3) (+ x 10) (+ y row))
                   (set! row (+ row 17)))
                 (split-in-chunks 3 (cell-options c))))
              (send dc set-font font-numbers)))

          (define (fill-content-cell bg fg)
            (fill-bg-cell bg)
            (cond
              [(equal? mode 'solved)
               (write-digit fg)]
              [else
               (if (> (cell-play c) 0)
                   (write-digit fg)
                   (write-options))]))

          (cond
            [(not (cell-mutable? c))
             (fill-content-cell "white smoke" "black")]
            [(and warnings (cell-invalid? c))
             (fill-content-cell "red" "yellow")]
            [else
             (fill-content-cell "white" "blue")])))

      (for-each draw-cell (list-of-cells)))

    ;; (define panel
    ;;   (new horizontal-panel%
    ;;        [parent main-frame]
    ;;        [alignment '(left center)]))

    ;; ; new-game
    ;; (new button% [parent panel]
    ;;      [label "New"]
    ;;      [min-width 150]
    ;;      [min-height 50]
    ;;      [vert-margin 25]
    ;;      [horiz-margin 12]
    ;;      [font font-buttons]
    ;;      [callback (lambda (button event)
    ;;                  (set! data-mode 'play)
    ;;                  (load-puzzle)
    ;;                  (set! cursor-cell (get-cell 5 5))
    ;;                  (send main-frame refresh)
    ;;                  (send my-canvas focus))])

    ;; ; save-game
    ;; (new button% [parent panel]
    ;;      [label "Save"]
    ;;      [min-width 150]
    ;;      [min-height 50]
    ;;      [vert-margin 25]
    ;;      [horiz-margin 12]
    ;;      [font font-buttons]
    ;;      [callback (lambda (button event)
    ;;                  (save-puzzle)
    ;;                  (send main-frame refresh)
    ;;                  (send my-canvas focus))])

    ;; (new check-box% [parent panel]
    ;;      [label "Feedback"]
    ;;      [min-width 200]
    ;;      [min-height 50]
    ;;      [vert-margin 25]
    ;;      [horiz-margin 12]
    ;;      [font font-buttons]
    ;;      [callback (lambda (c e)  ;; toggle warnings field
    ;;                  (set! warnings (not warnings))
    ;;                  (send main-frame refresh)
    ;;                  (send my-canvas focus))])

    ; util: testing
    (define/private (letter-test letter)
      (define text-size-dc (new bitmap-dc% [bitmap (make-object bitmap% 1 1)]))
      (send text-size-dc set-font font-numbers)
      (send text-size-dc get-text-extent letter))

    ))
