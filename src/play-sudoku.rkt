#! /usr/bin/env racket

#lang racket
;; play-sudoku.rkt

(require "sudoku.rkt")
(require "sudoku-gui.rkt")

(define (main)
  (let* ([sudoku (new sudoku%)]
         [sudoku-gui (new sudoku-gui% [sudoku sudoku])])
    (send sudoku new-puzzle)
    (send sudoku-gui start-gui)))

(main)
