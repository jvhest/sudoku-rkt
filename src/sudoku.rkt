#lang racket
;; File: sudoku.rkt

(require racket/gui/base)

(define *sudoku-board* '())

(define h 600)
(define w 600)

(define canvas-global '())
(define dc-global '())

;; main frame
(define frame (new frame% [label "Sudoku Puzzle"] [width w] [height (+ 100 h)]))

;; panel 1 (sudoku board)
(define panel1 (new horizontal-panel% [parent frame]
                                     [alignment '(center center)]
                                     [min-height h]
                                     [min-width w]
                                     [stretchable-width false]	 
                                     [stretchable-height false]))

(new canvas% [parent panel1]
             [min-height h]
             [min-width w]
             [paint-callback
              (lambda (canvas dc)
                (set! canvas-global canvas)
                (set! dc-global dc)
                (draw-grid canvas dc h)
                (draw-sub-grid canvas dc h)
                (print-puzzle *sudoku-board* 0 0))])

;;; Draw the main-grid structure on canvas.   
(define (draw-grid canvas dc s)
  (send dc set-pen "black" 4 'solid)
  (send dc draw-line (* 1/3 s) 0 (* 1/3 s) s)
  (send dc draw-line (* 2/3 s) 0 (* 2/3 s) s)
  (send dc draw-line 0 (* 1/3 s) s (* 1/3 s))
  (send dc draw-line 0 (* 2/3 s) s (* 2/3 s)))

;;; Draw the sub-grid structure on canvas.
(define (draw-sub-grid canvas dc s)
  (send dc set-pen "black" 1 'solid)
  (for ((i 9))
    (send dc draw-line (* (/ i 9) s) 0 (* (/ i 9) s) s)
    (send dc draw-line 0 (* (/ i 9) s) s (* (/ i 9) s))))

;;; Print entire table to canvas
(define (print-puzzle puzzle row column)
  (send dc-global set-font (make-font #:size 25 #:family 'roman #:weight 'bold))
  (p-nested-lst (car puzzle) (cadr puzzle) dc-global h row column))

(define sudoku-ex1
  (create-sudoku-obj
   '((0 0 0 9 7 0 0 0 0) 
    (0 4 0 2 5 0 1 0 7) 
    (0 0 7 6 0 0 4 0 3) 
    (0 1 2 8 0 0 6 0 0) 
    (9 7 0 0 4 0 0 3 5) 
    (0 0 4 0 0 2 9 1 0) 
    (2 0 1 0 0 7 5 0 0) 
    (4 0 9 0 8 1 0 6 0) 
    (0 0 0 0 2 9 0 0 0))))

(define (p-lst lst t-table y dc s row column)
  (for ((val lst) (x 9)) (print-ind val t-table x y dc s row column)))

(define (p-nested-lst lst t-table dc s row column)
  (for ((item lst) (y 9)) (p-lst item t-table y dc s row column)))

;;; Print each value in table based on position
(define (print-ind val t-table x y dc s row column)
  (define (helper col)
    (begin
      (if (and (= x (- column 1)) (= y (- row 1)))
          (send dc set-text-foreground "red")
          (send dc set-text-foreground col))
      
      (if (not (= 0 column))
          (begin 
            (send dc set-brush (new brush% [color "white"]))
            (let ((x (* (/ x 9) s)) (y (* (/ y 9) s)))
              (send dc set-pen "white" 1 'transparent)
              (send dc draw-rectangle (+ x 5) (+ y 5) (/ w 11) (/ h 11)))
            
            (send dc draw-text (if (equal? val " ") val (number->string val)) (+ (* (/ x 9) s) 20) (+ (* (/ y 9) s) 15)))
          
          (send dc draw-text (if (equal? val " ") val (number->string val)) (+ (* (/ x 9) s) 20) (+ (* (/ y 9) s) 15)))))
  
  (if (equal? val 0) (set! val " ") #f)
  (if (not (is-given? t-table x y))
      (helper "black")     
      (helper "green")))

;; panel 2 (buttons)
(define panel2 (new horizontal-panel% [parent frame]
                                     [alignment '(center bottom)]
                                     [min-height 100]
                                     [min-width 600]))

(define solve-button (new button% [parent panel2] 
                          [label "Solve"]
                          [min-width 290]
                          [min-height 100]
                          [callback  (lambda (button event) (begin 
                                                              (print-puzzle (step-solve-sudoku curr-sudoku #f) 0 0)
                                                              (send done-frame show #f)))]))

(define step-button (new button% [parent panel2] 
                          [label "Slow Solve"]
                          [min-width 290]
                          [min-height 100]
                          [callback  (lambda (button event) (begin
                                                              (print-puzzle (step-solve-sudoku curr-sudoku #t) 0 0)
                                                              (send done-frame show #f)))]))

;; floating message frame
(define done-frame (new frame% [label "Sudoku Complete!"] [width 300] [height 50]))
(define msg (new message% [parent done-frame]
                 [font (make-font #:size 25 #:family 'roman #:weight 'bold)]
                          [label "Your Sudoku Puzzle is Completed. Nice Work!"]))

(define style-delta (make-object style-delta% 
                                 'change-normal-color))

(define (show-window board)
  (set! *sudoku-board* board)
(send frame show #t)