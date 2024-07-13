#lang racket
;; main.rkt

(require board.rkt)
(require sudoku.rkt)

(require "tests/test-data.rkt")

(define *board-start* (create-sudoku-board my-puzzle))
(define *board-solved* (create-sudoku-board my-puzzle))

(define (setup-sudoku sudoku-puzzle)
  (define *board-start* (load-sudoku sudoku-puzzle))
  (define *board-solved* (solve-game *board-start*))
  (define *board-play* '()))

(define (main sudoku-path)
  (if (not (file-exists? sudoku-path))
      (error "sudoku-file does not exist")
      (let ([board (make-sudoku (with-input-from-file sudoku-path
                                  (lambda () (read-line))))])
        (solve-game board)
        ; ...
        (print-puzzle board))))
