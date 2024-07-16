#lang racket
;; sudoku.rkt

(provide sudoku%)

(define sudoku%
  (class object%
    (init puzzle)
    
    (field
     [easy #f]) ;; activate warning for invalid moves
     
    (define-values (cursor-r cursor-c) (values 5 5))

    (define data (setup-data puzzle))
    (define data-static (vector-copy data))
    (define data-solved (setup-data-solved))

    (define/public (show-solved)
      (set! data (vector-copy data-solved)))
          
    (define/public (is-static? row col)
      (> (vector-ref data-static (row-col->index row col)) 0))

    ;; natural natural -> natural
    (define/public (get-value row col)
      (vector-ref data (row-col->index row col)))

    ;; natural natural -> natural
    (define/public (get-value-solved row col)
      (vector-ref data-solved (row-col->index row col)))

    ;; natural natural natural -> void
    (define/public (set-value row col val)
      (let ([index (row-col->index row col)])
        (when (= 0 (vector-ref data-static index))
          (vector-set! data index val))))

    (define/public (set-cursor row col)
      (set!-values (cursor-r cursor-c) (values row col)))

    (define/public (get-cursor)
      (values cursor-r cursor-c))

    ;; natural natural -> natural
    (define/public (get-cursor-val)
      (vector-ref data (row-col->index cursor-r cursor-c)))

    ;; natural natural -> natural
    (define/public (get-cursor-solved)
      (vector-ref data-solved (row-col->index cursor-r cursor-c)))

    (define/public (set-cursor-val val)
      (set-value cursor-r cursor-c val))
    
    ;; direction oneof 'up 'down 'left 'right -> boolean (need refresh)
    (define/public (move-cursor key)
      (cond
        [(and (or (equal? key 'up) (equal? key #\k)) (> cursor-r 1))
         (set-cursor (sub1 cursor-r) cursor-c)]
        [(and (or (equal? key 'down) (equal? key #\j)) (< cursor-r 9))
         (set-cursor (add1 cursor-r) cursor-c)]
        [(and (or (equal? key 'left) (equal? key #\h)) (> cursor-c 1))
              (set-cursor cursor-r (sub1 cursor-c))]
        [(and (or (equal? key 'right) (equal? key #\l)) (< cursor-c 9))
              (set-cursor cursor-r (add1 cursor-c))]
        [else #f]))  ; no refresh needed

    ;; SudokuState -> Boolean
    ;; returns #t if the puzzle is solved, otherwise returns #f
    (define/public (solved?)
      (not (vector-member 0 data)))

    (define/public (print-puzzle)
      (displayln "\n-----------------------")
      (for ([r (in-range 1 10)])
        (display (format "~a -> " r))
        (for ([c (in-range 1 10)])
          (display (format "~a " (get-value r c))))
        (displayln ""))
      (display "     ")
      (for ([c (in-range 1 10)]) (display (format "~a " "|")))
      (display "\n     ")
      (for ([c (in-range 1 10)]) (display (format "~a " c)))
      (displayln "\n-----------------------"))

    ;; Private part
    (super-new)
    
    (define/private (setup-data puzzle)
      (let ([p (string-replace puzzle " " "")])
        (unless (= (string-length p) 81)
            (error "length of sudoku-puzzle is not 81"))
        (build-vector 81 (lambda (i) (char->int (string-ref p i))))))

    (define/private (setup-data-solved)
      (let ([d (vector-copy data)])
        (solve-game)  ; uses vector data to solve the game
        (print-puzzle)
        (let ([ds (vector-copy data)])
          (set! data d)  ; restore vector data from saved data
          ds)))
    
    (define/private (char->int c)
      (let ([val (- (char->integer c) 48)])
        (unless  (and (>= val 0) (<= val 9))
          (error "invalid value in puzzle-string"))
        val))

    ;; natural natural -> natural
    (define/private (row-col->index row col)
      (+ (* (sub1 row) 9) (sub1 col)))

    ;; SudokuState Column -> mutable-set of CellVal
    ;; interp. Collect all CellVal's in this Column in a mutable-set.
    (define/private (find-options-row row)
      (let ([options (mutable-set)])
        (for ([col (in-range 1 10)])
          (set-add! options (get-value row col)))
        options))

    ;; SudokuState Column -> mutable-set of CellVal
    ;; interp. Collect all CellVal's in this Column in a mutable-set.
    (define/private (find-options-col col)
      (let ([options (mutable-set)])
        (for ([row (in-range 1 10)])
          (set-add! options (get-value row col)))
        options))

    ;; SudokuState Row Column -> mutable-set of CellVal
    ;; interp. Collect all CellVal's in MiniGrid with Row Column in a mutable-set.
    (define/private (find-options-minigrid row col)
      (let ([r0 (+ (* (floor (/ (- row 1) 3)) 3) 1)]  ;; first row of minigrid
            [c0 (+ (* (floor (/ (- col 1) 3)) 3) 1)]  ;; first column of minigrid
            [options (mutable-set)])
        (for ([row (in-range r0 (+ r0 3))])
          (for ([col (in-range c0 (+ c0 3))])
            (set-add! options (get-value row col))))
        options))

    ;; SudokuState Row Column -> mutable-set of CellVal
    ;; interp. Collect all valid CellVal options for Cell[Row, Column] in a mutable-set.
    (define/private (find-options row col)
      (let ([options (mutable-set 1 2 3 4 5 6 7 8 9)]
            [used (mutable-set)])
        (set-union! used (find-options-row row)
                    (find-options-col col)
                    (find-options-minigrid row col))
        (set-subtract! options used (set 0))
        options))

    ;; SudokuState -> (Row Col)
    ;; interp. Find next unsolved Cell in data-solved
    (define/private (next-to-solve-cell)
      (if (vector-member 0 data)
          (begin
            (let-values ([(row col) (quotient/remainder (vector-member 0 data) 9)])
              (values (add1 row) (add1 col))))
          (values -1 -1)))

    ;; SudokuState -> Boolean
    ;; interp. Recursively solve the sudoku from 1 empty cell to the next.
    (define/private (solve)
      ;; (log "= recursion =")
      (let-values ([(row col) (next-to-solve-cell)])
        (cond
          ;; [(equal? (list row col) (list -1 -1)) #t]  ; no more zero values
          [(solved?) #t]  ; no more zero values
          [else
           (if (for/or ([val (in-mutable-set (find-options row col))])
                 ;; (log (format "(~a, ~a) ==> ~a" row col (get-value row col)))
                 ;; (log (find-options row col))
                 ;; (log (format "(~a, ~a) -> ~a" row col val))
                 (set-value row col val)
                 (solve))
               #t
               (begin
                 ;; (log "i was wrong")
                 (set-value row col 0)
                 #f))])))

    ;; SudokuState -> Boolean
    ;; interp. Solve sudoku Cell's with only 1 option. After that Backtracking.
    (define/public (solve-game)
      (let ([options null])
        (for ([row (in-range 1 10)])
          (for ([col (in-range 1 10)])
            (when (equal? (get-value row col) 0)
              (set! options (find-options row col))
              (when (equal? (set-count options) 1)
                (set-value row col (set-first options))))))
        (solve)))
    ))

;; (define test (new sudoku% (puzzle my-puzzle)))
;; (send test print-puzzle)
;; (send test solve-game)
;; (send test print-puzzle)
