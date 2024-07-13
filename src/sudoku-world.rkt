#lang racket
;; sudoku-world.rkt

(provide CURR-SUDOKU
         make-sudoku
         get-value
         set-value)

(require "../tests/test-data.rkt")

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
;; (define curr-sudoku (make-vector 81 0))

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

(define CURR-SUDOKU (make-sudoku my-puzzle))
