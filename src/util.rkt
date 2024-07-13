#lang racket

(provide log)

(define *log-active* #f)

(define (log message)
  (when *log-active*
    (displayln message)))

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
