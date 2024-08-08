#! /usr/bin/env racket

#lang racket
;; main.rkt

;;(require "sudoku.rkt")
(require "gui.rkt")

(define (main)
  (let ([sudoku-gui (new sudoku-gui%)])
    (send sudoku-gui start-gui)))

(main)
