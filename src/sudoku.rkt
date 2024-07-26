#lang racket
;; sudoku.rkt

(provide sudoku%)

(require racket/gui/base)
(require racket/draw)

(define sudoku%
  (class object%
    
    (define warnings #f)
    ;; activate warning for invalid moves
    (define/public (toggle-warnings)
      (set! warnings (not warnings)))

    (define/public (show-warnings?) warnings)

    (define cursor-r 5)
    (define cursor-c 5)

    (define/public (set-cursor row col)
      (set!-values (cursor-r cursor-c) (values row col)))

    (define/public (get-cursor)
      (values cursor-r cursor-c))

    ;; direction oneof 'up 'down 'left 'right -> boolean (#t need refresh)
    (define/public (move-cursor key)
      (cond
        [(and (equal? key 'up) (> cursor-r 1))
         (set-cursor (sub1 cursor-r) cursor-c)]
        [(and (equal? key 'down) (< cursor-r 9))
         (set-cursor (add1 cursor-r) cursor-c)]
        [(and (equal? key 'left) (> cursor-c 1))
         (set-cursor cursor-r (sub1 cursor-c))]
        [(and (equal? key 'right) (< cursor-c 9))
         (set-cursor cursor-r (add1 cursor-c))]
        [else #f]))  ; no refresh needed

    (define curr-file "")
    (define curr-dir "")

    (define data null)
    (define data-static null)
    (define data-solved null)

    (define/public (new-puzzle)
      (let ([puzzles (read-from-file)])
        ;; setup initial (static) data (puzzle to solve)
        (set! data-static (string->vector (first puzzles)))  ; initial puzzle

        ;; setup game data (where game is played)
        (set! data (string->vector
                    (if (= (length puzzles) 2)
                        (second puzzles)
                        (first puzzles))))

        ;; setup solution date as reference in game
        (set! data-solved (string->vector (first puzzles)))  ; initial puzzle
        (if (not (solve-game))
            (error "puzzle can not be solved!!")
            (print-puzzle))

        ;; reset cursor to middle of board
        (set-cursor 5 5)))

    (define/public (save-puzzle)
      (let ([string-list (list (vector->string data-static) (vector->string data))])
        (write-to-file string-list)))
    
    (define/public (show-solved)
      (set! data (vector-copy data-solved)))
          
    (define/public (is-static? row col)
      (> (vector-ref data-static (row-col->index row col)) 0))

    ;; natural natural -> natural
    (define/public (get-value row col #:mode [mode 'play])
      (unless (member mode '(static play solved))
        (error "not a valid mode value"))
      (cond
        [(equal? mode 'static)
         (vector-ref data-static (row-col->index row col))]
        [(equal? mode 'solved)
         (vector-ref data-solved (row-col->index row col))]
        [else (vector-ref data (row-col->index row col))]))

    ;; natural natural natural -> void
    (define/public (set-value row col val #:mode [mode 'play])
      (unless (member mode '(play solved))
        (error "not a valid mode value"))
      (when (char? val)
        (set! val (char->digit val)))
      (let ([index (row-col->index row col)])
        (when (= 0 (vector-ref data-static index))
          (cond
            [(equal? mode 'solved)
             (vector-set! data-solved index val)]
            [else
             (vector-set! data index val)]))))

    (define/public (is-invalid? digit row col)
      (and (> digit 0) (<= digit 9)
           (not (equal? digit (get-value row col #:mode 'solved)))))
    
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
    
    (define/private (read-from-file)  ; Void -> list of String
      (let ([fname (if (equal? curr-dir "")
                       (get-file)
                       (get-file #f #f curr-dir))]
            [new-list '()]
            [temp '()])
        (if fname
            (begin
              (set!-values (curr-dir curr-file temp) (split-path fname))
              (call-with-input-file fname
                (lambda (port)
                  (for ([l (in-lines port)])
                    (set! new-list (cons l new-list)))
                  (reverse new-list))))
            (error "no file selected!!"))))

    (define/private (string->vector puzzle)
      (let ([p (string-replace puzzle " " "")])
        (unless (= (string-length p) 81)
          (error "length of sudoku-puzzle is not 81"))
        (build-vector 81 (lambda (i) (char->digit (string-ref p i))))))

    (define (vector->string v) ; Vector -> String
      (let ([char-list '()])
        (for ([d v])
          (set! char-list (cons (digit->char d) char-list)))
        (list->string (reverse char-list))))

    (define/private (write-to-file string-list)  ; list of String -> Void
      (let ([fname (put-file #f #f curr-dir curr-file)])
        (if fname 
            (call-with-output-file fname
              (lambda (port)
                (for ([s (in-list string-list)])
                  (displayln s port)))
              #:exists 'truncate/replace)
            (error "no file selected!!"))))
      
    (define/private (char->digit c)
      (let ([val (- (char->integer c) 48)])
        (unless  (and (>= val 0) (<= val 9))
          (error "invalid value in puzzle-string"))
        val))

    (define/private (digit->char d)
      (integer->char (+ d 48)))
    
    ;; natural natural -> natural
    (define/private (row-col->index row col)
      (+ (* (sub1 row) 9) (sub1 col)))

    ;; SudokuState Column -> mutable-set of CellVal
    ;; interp. Collect all CellVal's in this Column in a mutable-set.
    (define/private (find-options-row row #:mode [mode 'solved])
      (let ([options (mutable-set)])
        (for ([col (in-range 1 10)])
          (set-add! options (get-value row col #:mode mode)))
        options))

    ;; SudokuState Column -> mutable-set of CellVal
    ;; interp. Collect all CellVal's in this Column in a mutable-set.
    (define/private (find-options-col col #:mode [mode 'solved])
      (let ([options (mutable-set)])
        (for ([row (in-range 1 10)])
          (set-add! options (get-value row col #:mode mode)))
        options))

    ;; SudokuState Row Column -> mutable-set of CellVal
    ;; interp. Collect all CellVal's in MiniGrid with Row Column in a mutable-set.
    (define/private (find-options-minigrid row col #:mode [mode 'solved])
      (let ([r0 (+ (* (floor (/ (- row 1) 3)) 3) 1)]  ;; first row of minigrid
            [c0 (+ (* (floor (/ (- col 1) 3)) 3) 1)]  ;; first column of minigrid
            [options (mutable-set)])
        (for ([row (in-range r0 (+ r0 3))])
          (for ([col (in-range c0 (+ c0 3))])
            (set-add! options (get-value row col #:mode mode))))
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
      (if (vector-member 0 data-solved)
          (begin
            (let-values ([(row col) (quotient/remainder (vector-member 0 data-solved) 9)])
              (values (add1 row) (add1 col))))
          (values -1 -1)))

    ;; SudokuState -> Boolean
    ;; interp. Recursively solve the sudoku from 1 empty cell to the next.
    (define/private (solve)
      ;; (log "= recursion =")
      (let-values ([(row col) (next-to-solve-cell)])
        (cond
          [(not (vector-member 0 data-solved)) #t]  ; no more zero values
          [else
           (if (for/or ([val (in-mutable-set (find-options row col))])
                 ;; (log (format "(~a, ~a) ==> ~a" row col (get-value row col)))
                 ;; (log (find-options row col))
                 ;; (log (format "(~a, ~a) -> ~a" row col val))
                 (set-value row col val #:mode 'solved)
                 (solve))
               #t
               (begin
                 ;; (log "i was wrong")
                 (set-value row col 0 #:mode 'solved)
                 #f))])))

    ;; SudokuState -> Boolean
    ;; interp. Solve sudoku Cell's with only 1 option. After that Backtracking.
    (define/public (solve-game)
      (let ([options null])
        (for ([row (in-range 1 10)])
          (for ([col (in-range 1 10)])
            (when (equal? (get-value row col #:mode 'solved) 0)
              (set! options (find-options row col))
              (when (equal? (set-count options) 1)
                (set-value row col (set-first options))))))
        (solve)))
    ))

;; (define test (new sudoku% (puzzle my-puzzle)))
;; (send test print-puzzle)
;; (send test solve-game)
;; (send test print-puzzle)
