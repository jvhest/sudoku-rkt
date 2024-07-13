#lang racket
;; board.rkt

(provide solve-game)

(require "sudoku-world.rkt")
(require "util.rkt")

(require "../tests/test-data.rkt")

(require rackunit)
(require rackunit/text-ui)


;; SudokuState Row -> mutable-set of CellVal
;; interp. Collect all CellVal's in this Row in a mutable-set.
(module+ test
  (test-begin
   (let ([sudoku (make-sudoku my-puzzle)])
     (print-puzzle sudoku)
     (check-equal? (find-options-row sudoku 3) (mutable-set 0 1 4 6 8))
     (check-equal? (find-options-row sudoku 9) (mutable-set 0 1 3 5))
     (check-equal? (find-options-row sudoku 6) (mutable-set 0 2 6 7 8)))))

(define (find-options-row sudoku row)
  (let ([options (mutable-set)])
    (for ([col (in-range 1 10)])
      (set-add! options (get-value sudoku row col)))
    options))

;; SudokuState Column -> mutable-set of CellVal
;; interp. Collect all CellVal's in this Column in a mutable-set.
(module+ test
  (test-begin
   (let ([sudoku (make-sudoku my-puzzle)])
     (check-equal? (find-options-col sudoku 1) (mutable-set 0 7 8 9))
     (check-equal? (find-options-col sudoku 5) (mutable-set 0 1 2))
     (check-equal? (find-options-col sudoku 8) (mutable-set 0)))))

(define (find-options-col sudoku col)
  (let ([options (mutable-set)])
    (for ([row (in-range 1 10)])
      (set-add! options (get-value sudoku row col)))
    options))

;; SudokuState Row Column -> mutable-set of CellVal
;; interp. Collect all CellVal's in MiniGrid with Row Column in a mutable-set.
(module+ test
  (test-begin
   (let ([sudoku (make-sudoku my-puzzle)])
     (check-equal? (find-options-minigrid sudoku 3 3) (mutable-set 0 1 3 9))
     (check-equal? (find-options-minigrid sudoku 5 9) (mutable-set 0 2 8 9))
     (check-equal? (find-options-minigrid sudoku 8 4) (mutable-set 0 1 2 3 6 9)))))

(define (find-options-minigrid sudoku row col)
  (let ([r0 (+ (* (floor (/ (- row 1) 3)) 3) 1)]  ;; first row of minigrid
        [c0 (+ (* (floor (/ (- col 1) 3)) 3) 1)]  ;; first column of minigrid
        [options (mutable-set)])
    (for ([row (in-range r0 (+ r0 3))])
      (for ([col (in-range c0 (+ c0 3))])
        (set-add! options (get-value sudoku row col))))
    options))

;; SudokuState Row Column -> mutable-set of CellVal
;; interp. Collect all valid CellVal options for Cell[Row, Column] in a mutable-set.
(module+ test
  (test-begin
   (let ([sudoku (make-sudoku my-puzzle)])
     (check-equal? (find-options sudoku 2 3) (mutable-set 4 7))
     (check-equal? (find-options sudoku 5 4) (mutable-set 4 5 9))
     (check-equal? (find-options sudoku 9 8) (mutable-set 2 4 6 7 8)))))

(define (find-options sudoku row col)
  (let ([options (mutable-set 1 2 3 4 5 6 7 8 9)]
        [used (mutable-set)])
    (set-union! used (find-options-row sudoku row)
                (find-options-col sudoku col)
                (find-options-minigrid sudoku row col))
    (set-subtract! options used (set 0))
    options))


;; SudokuState -> Boolean
;; returns #t if the puzzle is solved, otherwise returns #f
(module+ test
  (test-begin
    (let ([sudoku-unsolved (make-sudoku my-puzzle)]
          [sudoku-solved (make-sudoku my-puzzle-solved)])
     (check-equal? (solved? sudoku-unsolved) #f)
     (check-equal? (solved? sudoku-solved) #t))))

(define (solved? sudoku)
  (not (vector-member 0 sudoku)))


;; SudokuState -> (Row Col)
;; interp. Find next unsolved Cell
(module+ test
  (test-begin
    (let ([sudoku-unsolved (make-sudoku my-puzzle)]
          [sudoku-solved (make-sudoku my-puzzle-solved)])
      (check-equal? (let-values ([(r c)
                                  (next-to-solve-cell sudoku-unsolved)]) (list r c)) (list 1 1))
      (check-equal? (let-values ([(r c)
                                  (next-to-solve-cell sudoku-solved)]) (list r c)) (list -1 -1)))))

(define (next-to-solve-cell sudoku)
  (if (vector-member 0 sudoku)
      (begin
        (let-values ([(row col) (quotient/remainder (vector-member 0 sudoku) 9)])
          (values (add1 row) (add1 col))))
      (values -1 -1))) 

;; SudokuState -> Boolean
;; interp. Recursively solve the sudoku from 1 empty cell to the next.
(module+ test
  (test-begin
    (let ([sudoku-unsolved (make-sudoku my-puzzle)]
          [sudoku-solved (make-sudoku my-puzzle-solved)])
     (check-equal? (solve sudoku-unsolved) #t)
     (check-equal? sudoku-unsolved sudoku-solved))))

(define (solve sudoku)
  (log "= recursion =")
  (let-values ([(row col) (next-to-solve-cell sudoku)])
    (cond
      ;; [(equal? (list row col) (list -1 -1)) #t]  ; no more zero values
      [(solved? sudoku) #t]  ; no more zero values
      [else
       (if (for/or ([val (in-mutable-set (find-options sudoku row col))])
             (log (format "(~a, ~a) ==> ~a" row col (get-value sudoku row col)))
             (log (find-options sudoku row col))
             (log (format "(~a, ~a) -> ~a" row col val)) 
             (set-value sudoku row col val)
             (solve sudoku))
           #t
           (begin 
             (log "i was wrong")
             (set-value sudoku row col 0)
             #f))])))


;; SudokuState -> Boolean
;; interp. Solve sudoku Cell's with only 1 option. After that Backtracking.
(module+ test
  (test-begin
    (let ([sudoku-unsolved (make-sudoku my-puzzle)]
          [sudoku-solved (make-sudoku my-puzzle-solved)])
     (check-equal? (solve-game sudoku-unsolved) #t)
     (check-equal? sudoku-unsolved sudoku-solved))))

(define (solve-game sudoku)
  (let ([options null])
    (for ([row (in-range 1 10)])
      (for ([col (in-range 1 10)])
        (when (equal? (get-value sudoku row col) 0)
          (set! options (find-options sudoku row col))
          (when (equal? (set-count options) 1)
            (set-value sudoku row col (set-first options))))))
    (solve sudoku)))

(define (print-puzzle puzzle)
  (displayln "\n-----------------------")
  (for ([r (in-range 1 10)])
    (display (format "~a -> " r))
    (for ([c (in-range 1 10)])
      (display (format "~a " (get-value puzzle r c))))
    (displayln ""))
  (display "     ")
  (for ([c (in-range 1 10)]) (display (format "~a " "|")))
  (display "\n     ")
  (for ([c (in-range 1 10)]) (display (format "~a " c)))
  (displayln "\n-----------------------"))
