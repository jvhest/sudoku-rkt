#lang racket
;; main.rkt

(require "sudoku.rkt")
(require "sudoku-gui.rkt")

(require "../tests/test-data.rkt")

(define (main sudoku-path)
  (if (not (file-exists? sudoku-path))
      (error "sudoku-file does not exist")
      (let* ([sudoku (new sudoku% [puzzle
                                   (with-input-from-file sudoku-path
                                     (lambda () (read-line)))])]
             [sudoku-gui (new sudoku-gui% [sudoku sudoku])])
        (send sudoku-gui show-frame))))
        
(main "../tests/sudoku-puzzle-3.txt")