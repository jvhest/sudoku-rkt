#lang racket
;; main.rkt

(require board-org.rkt)
(require sudoku.rkt)

(require "tests/test-data.rkt")

(define *board-start* (create-sudoku-board my-puzzle))
(define *board-solved* (create-sudoku-board my-puzzle))

(define (setup-sudoku sudoku-puzzle)
  (define *board-start* (load-sudoku sudoku-puzzle))
  (define *board-solved* (solve-game *board-start*))
  (define *board-play* '()))