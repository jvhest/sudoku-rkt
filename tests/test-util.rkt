#lang racket
;; File: test-util.rkt
(require rackunit)
(require rackunit/text-ui)

(require "../util.rkt")
(require "../tests/test-data.rkt")

;; test loading of puzzle in sudoku-board
(check-equal? (locate-value *puzzle* 1 1) 0)
(check-equal? (locate-value *puzzle* 1 3) 3)
(check-equal? (locate-value *puzzle* 2 1) 9)
(check-equal? (locate-value *puzzle* 7 7) 5)
(check-equal? (locate-value *puzzle* 9 9) 0)

;; test solved?
(check-equal? (solved? *puzzle*) #f)
(check-equal? (solved? *puzzle-solved*) #t)