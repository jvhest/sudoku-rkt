#lang racket
;; board-org.rkt

(provide solve-game)

(require "util.rkt")
(require rackunit)
(require rackunit/text-ui)

;; Recursive sudoku solver
;;
;; A Sudoku contains a 9×9 grid, which is divided into nine smaller 3×3
;; grids (known as minigrids).
;; The aim of the game is to place a number from 1 to 9 into each of the cells,
;; such that each number must appear exactly once in each row and in each column
;; in the grid. Additionally, each minigrid must contain all the numbers 1
;; through 9.
;; At the start of the game the grid is partially filled with a starter-puzzle.

;; SudokuState is vector of Cells
;; interp. The 9x9 grid is represented by a vector with a length of 81 (zero-
;; based index 0..80).
;; The numbering of the rows and columns in the grid is from 1 to 9, starting
;; from top-left cell. The location of a cell is indicated by (row,col) pair. 
;; The starter-puzzle will be read from a string with length 81.

;; CellVal is Natural[0..9]
;; Where:
;;   0 indicates that the cell is still empty.
;;  [1,9] are valid sudoku values

;; Row is Natural[1..9]
;; Where:
;; 1 is the first row of the grid
;; 9 is the last row of the grid

;; Column is Natural[1..9]
;; Where:
;; 1 is the first column of the grid
;; 9 is the last column of the grid

;; Pos is Natural[0..80]
;; Where:
;; 0 is the first slot in SudokuState vector
;; 80 is the last slot in SudokuState vector

;; Char -> CellVal
;; interp. convert Char to CellVal and check if in range 0..9
(module+ test
   (check-equal? (char->int #\0) 0)
   (check-equal? (char->int #\5) 5)
   (check-equal? (char->int #\9) 9)
   (check-exn exn:fail? 
              (lambda () (char->int #\a))))

(define (char->int c)
  (let ([val (- (char->integer c) 48)])
    (unless  (and (>= val 0) (<= val 9))
      (error "invalid value in puzzle-string"))
    val))

;; String -> SudokuState
;; interp. create SudokuState from string of 81 chars range [#\0..#\9]
(module+ test
  (test-begin
   (let ([sudoku (make-sudoku my-puzzle)])
     (check-equal? (vector-ref sudoku 2) 3)
     (check-equal? (vector-ref sudoku 17) 1)
     (check-equal? (vector-ref sudoku 36) 7)
     (check-equal? (vector-ref sudoku 52) 0))))
     
(define (make-sudoku puzzle)
  (unless (= (string-length puzzle) 81)
    (error "length of sudoku-puzzle is not 81"))
  (let ([data (make-vector 81 0)])
    (build-vector 81 (lambda (i) (char->int (string-ref puzzle i))))))

;; Pos -> Row Column
;; interp. Given the index in the vector calculate the (row, col) position
;; in the grid
(module+ test
  (test-begin
   (check-equal? (let-values ([(r c) (index->row-col 2)]) (list r c)) (list 1 3))
   (check-equal? (let-values ([(r c) (index->row-col 17)]) (list r c)) (list 2 9))
   (check-equal? (let-values ([(r c) (index->row-col 36)]) (list r c)) (list 5 1))
   (check-equal? (let-values ([(r c) (index->row-col 52)]) (list r c)) (list 6 8))))

(define (index->row-col pos)
  (let-values ([(row col) (quotient/remainder pos 9)])
    (values (add1 row) (add1 col))))

;; Row Column -> Pos
;; interp. Given the row, col position in grid calculate index in the vector.
(module+ test
  (test-begin
   (check-equal? (row-col->index 1 3) 2)
   (check-equal? (row-col->index 2 9) 17)
   (check-equal? (row-col->index 5 1) 36)
   (check-equal? (row-col->index 6 8) 52)))

(define (row-col->index row col)
  (+ (* (sub1 row) 9) (sub1 col)))

;; Row Column -> CellVal
;; interp. Retrieve value from vector for grid-position row, col 
(module+ test
  (test-begin
   (let ([sudoku (make-sudoku my-puzzle)])
     (check-equal? (get-value sudoku 1 7) 6)
     (check-equal? (get-value sudoku 5 9) 8)
     (check-equal? (get-value sudoku 6 4) 7)
     (check-equal? (get-value sudoku 9 6) 0))))

(define (get-value sudoku row col)
  (vector-ref sudoku (row-col->index row col)))

;; Sudoku Row Column CellVal -> null
;; interp. Set value in SudokuState (vector) for grid-position row, col 
(module+ test
  (test-begin
   (let ([sudoku (make-sudoku my-puzzle)])
     (set-value sudoku 1 7 3) 
     (check-equal? (get-value sudoku 1 7) 3)
     (set-value sudoku 5 9 6) 
     (check-equal? (get-value sudoku 5 9) 6)
     (set-value sudoku 6 4 0) 
     (check-equal? (get-value sudoku 6 4) 0)
     (set-value sudoku 9 6 8) 
     (check-equal? (get-value sudoku 9 6) 8))))

(define (set-value sudoku row col val)
  (vector-set! sudoku (row-col->index row col) val))

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


#;
;;!!!
(define (valid-row? sudoku row)
  (let ([values (mutable-set)])
    (for/and ([col (in-range 1 10)])
      (if (not (set-member? values (get-value sudoku row col)))
          (begin (set-add! values (get-value sudoku row col)) #true)
          #false))))

#;
;!!!
(define (valid-sudoku? sudoku)
  #true)
  
  
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

(define (main sudoku-path)
  (if (not (file-exists? sudoku-path))
      (error "sudoku-file does not exist")
      (let ([board (make-sudoku (with-input-from-file sudoku-path
                                  (lambda () (read-line))))])
        (solve-game board)
        ; ...
        (print-puzzle board))))

;; -----------------------------------------------------------------------------

(module+ test


  (require "tests/test-data.rkt")

  (define *board* (make-sudoku my-puzzle))
  (define *board-solved* (make-sudoku my-puzzle-solved))
  (define *board-invalid* (make-sudoku invalid-puzzle))

  ;; (check-equal? (valid-row? *board* 1) #true)
  ;; (check-equal? (valid-row? *board-invalid* 1) #false)
  
  ;; test find all valid options in row
  (check-equal? (find-options-row *board* 1) (mutable-set 0 2 3 6))
  (check-equal? (find-options-row *board* 3) (mutable-set 0 1 4 6 8))
  (check-equal? (find-options-row *board* 9) (mutable-set 0 1 3 5))

  ;; test find all valid options in column
  (check-equal? (find-options-col *board* 1) (mutable-set 0 9 7 8))
  (check-equal? (find-options-col *board* 4) (mutable-set 0 3 8 1 7 6 2))
  (check-equal? (find-options-col *board* 9) (mutable-set 0 1 8 9))

  ;; test find all valid options in square
  (check-equal? (find-options-minigrid *board* 2 3) (mutable-set 0 3 9 1))
  (check-equal? (find-options-minigrid *board* 5 4) (mutable-set 0 1 2 7 8))
  (check-equal? (find-options-minigrid *board* 9 7) (mutable-set 0 5 9 3))
  
  ;; test find all options for one sudoku cell
  (check-equal? (find-options *board* 2 3) (mutable-set 4 7))
  (check-equal? (find-options *board* 5 4) (mutable-set 4 5 9))
  (check-equal? (find-options *board* 9 8) (mutable-set 2 4 6 7 8))
  
  ;; test solve-game
  (solve-game *board*)
  (check-equal? *board* *board-solved* "solving my-puzzle")

  (define *board2* (make-sudoku hard-puzzle))
  (define *board2-solved* (make-sudoku hard-puzzle-solved))
  (solve-game *board2*)
  (check-equal? *board2* *board2-solved* "solving hard-puzzle"))

