#lang racket

(provide (struct-out cell)
         get-cell
         list-of-cells
         load-puzzle
         save-puzzle
         curr-file
         update-options-for-cells
         solved?
         solved-with-errors
         print-3grid
         print-options)

(require racket/gui/base)

(require "util.rkt")

; -------------------
; grid data
;--------------------

(define-struct cell
  [static
   play
   solved
   invalid?
   mutable?
   options
   x
   y
   z]
  #:mutable)

; Natural (0..80) -> Cell
; interp. create cell for slot n in vector
(define (create-new-cell n)
  (make-cell 0 0 0 #f #t '()
             (add1 (remainder n 9))
             (add1 (quotient n 9))
             (index->mini-grid n)))

(define grid-of-cells
  (list->vector (map create-new-cell (build-list 81 values))))

(define (list-of-cells)
   (vector->list grid-of-cells))

(define (get-cell row col)
  (vector-ref grid-of-cells  (+ (* (sub1 row) 9) (sub1 col))))

(define (load-puzzle)
  (let ([puzzles (read-from-file)])
    (map (λ (s) (string-replace s " " "")) puzzles)
    (unless (= (string-length (first puzzles)) 81)
      (error "length of sudoku-puzzle is not 81"))
    (for ([i (in-range 81)])
      (let ([c (vector-ref grid-of-cells i)])
        (set-cell-static! c
          (char->digit (string-ref (first puzzles) i)))
        (set-cell-play! c
          (char->digit (string-ref (if (> (length puzzles) 1)
                                       (second puzzles)
                                       (first puzzles)) i)))
        (set-cell-solved! c
          (char->digit (string-ref (first puzzles) i)))
        (set-cell-mutable?! c
          (= (cell-static (vector-ref grid-of-cells i)) 0))
        (set-cell-invalid?! c #f))))
  (solve-game)
  (update-isvalid-for-cells)
  (update-options-for-cells)
  (print-puzzle))

(define (update-options-for-cells)
  (for ([c (filter(λ (c) (= (cell-play c) 0)) (vector->list grid-of-cells))])
    (set-cell-options! c (sort (find-options c 'play) <))))

(define (update-isvalid-for-cells)
  (for ([c (filter (λ (c) (and (= (cell-static c) 0) (> (cell-play c) 0)))
                   (vector->list grid-of-cells))])
    (set-cell-invalid?! c (not (= (cell-play c) (cell-solved c))))))

(define (save-puzzle)
  (let ([string-list
         (list
          (apply string (map (λ(c) (digit->char (cell-static c)))
                             (vector->list grid-of-cells)))
          (apply string (map (λ(c) (digit->char (cell-play c)))
                             (vector->list grid-of-cells))))])
     (write-to-file string-list)))

(define (solved?)
  (= (length (filter (λ (c) (= (cell-solved c) 0)) (vector->list grid-of-cells))) 0))

(define (solved-with-errors)
  (foldl (λ (c result) (+ result (if (= (cell-play c) (cell-solved c)) 0 1))) 0
         (filter (λ (c) (= (cell-static c) 0)) (vector->list grid-of-cells))))

;; natural natural -> natural
;; (define (cell-get-value row col #:mode [mode 'play])
;;   (unless (member mode '(static play solved))
;;     (error "not a valid mode value"))
;;   (let ([c (get-cell row col)])
;;     (cond
;;       [(equal? mode 'static) (cell-static c)]
;;       [(equal? mode 'play) (cell-play c)]
;;       [(equal? mode 'solved) (cell-solved c)])))

;; natural natural natural -> void
;; (define (cell-set-value row col val)
;;   (let ([c (get-cell row col)])
;;     (when (cell-mutable? c)  ; cell is mutable
;;       (set-cell-play! c val)
;;       (unless (and (> (cell-play c) 0)
;;                    (= (cell-play c) (cell-solved c)))
;;         (set-cell-invalid?! c #t)))))

; -------------------
; I/O
;--------------------

(define curr-file "")
(define curr-dir "")

(define (read-from-file)  ; Void -> list of String
  (let ([fname (if (equal? curr-dir "")
                   (get-file)
                   (get-file #f #f curr-dir))]
        [temp #f])
    (if fname
        (begin
          (set!-values (curr-dir curr-file temp) (split-path fname))
          (call-with-input-file fname
            (λ (port)
              (map (λ (s) (string-replace s " " ""))
                   (sequence->list (in-lines port))))))
        (error "no file selected!!"))))

(define (write-to-file string-list)  ; list of String -> Void
  (let ([fname (put-file #f #f curr-dir curr-file)]
        [temp #f])
    (if fname
        (begin
        (set!-values (curr-dir curr-file temp) (split-path fname))
        (call-with-output-file fname
          (lambda (port)
            (for ([s (in-list string-list)])
              (displayln s port)))
          #:exists 'replace))
        (error "no file selected!!"))))

; -------------------
; solver
;--------------------

(define mini-grid->index-table
'#((0 1 2 9 10 11 18 19 20)
   (3 4 5 12 13 14 21 22 23)
   (6 7 8 15 16 17 24 25 26)
   (27 28 29 36 37 38 45 46 47)
   (30 31 32 39 40 41 48 49 50)
   (33 34 35 42 43 44 51 52 53)
   (54 55 56 63 64 65 72 73 74)
   (57 58 59 66 67 68 75 76 77)
   (60 61 62 69 70 71 78 79 80)))

(define column->index-table
  '#((0 9 18 27 36 45 54 63 72)
   (1 10 19 28 37 46 55 64 73)
   (2 11 20 29 38 47 56 65 74)
   (3 12 21 30 39 48 57 66 75)
   (4 13 22 31 40 49 58 67 76)
   (5 14 23 32 41 50 59 68 77)
   (6 15 24 33 42 51 60 69 78)
   (7 16 25 34 43 52 61 70 79)
   (8 17 26 35 44 53 62 71 80)))

(define row->index-table
'#((0 1 2 3 4 5 6 7 8)
   (9 10 11 12 13 14 15 16 17)
   (18 19 20 21 22 23 24 25 26)
   (27 28 29 30 31 32 33 34 35)
   (36 37 38 39 40 41 42 43 44)
   (45 46 47 48 49 50 51 52 53)
   (54 55 56 57 58 59 60 61 62)
   (63 64 65 66 67 68 69 70 71)
   (72 73 74 75 76 77 78 79 80)))

;; SudokuState Column -> mutable-set of CellVal
;; interp. Collect all CellVal's in this Column in a mutable-set.
(define (find-options-row c mode)
    (map (λ (x) ((if (equal? mode 'solved) cell-solved cell-play) (vector-ref grid-of-cells x)))
                    (vector-ref row->index-table (sub1 (cell-y c)))))

;; SudokuState Column -> mutable-set of CellVal
;; interp. Collect all CellVal's in this Column in a mutable-set.
(define (find-options-col c mode)
    (map (λ (x) ((if (equal? mode 'solved) cell-solved cell-play) (vector-ref grid-of-cells x)))
                    (vector-ref column->index-table (sub1 (cell-x c)))))

(define (find-options-minigrid c mode)
    (map (λ (x) ((if (equal? mode 'solved) cell-solved cell-play) (vector-ref grid-of-cells x)))
                    (vector-ref mini-grid->index-table (sub1 (cell-z c)))))

;; SudokuState Row Column -> mutable-set of CellVal
;; interp. Collect all valid CellVal options for Cell[Row, Column] in a mutable-set.
(define (find-options c mode)
  (let ([options (mutable-set 1 2 3 4 5 6 7 8 9)]
        [used (list->set
               (append (find-options-row c mode)
                       (find-options-col c mode)
                       (find-options-minigrid c mode)))])
    (set-subtract! options used (set 0))
;    options))
    (set->list options)))

; -> cell
; scan grid-of-cells for cells with cell-solved 0
(define (next-to-solve-cell)
  (let ([unsolved-cells
         (filter (λ (c) (= (cell-solved c) 0)) (vector->list grid-of-cells))])
    (if (= (length unsolved-cells) 0)
        #f
        (first unsolved-cells))))

;; SudokuState -> Boolean
;; interp. Recursively solve the sudoku from 1 empty cell to the next.
(define (solve)
  ;;(log "= recursion =")
  (let ([c (next-to-solve-cell)])
    (cond
      [(not c) #t]  ; no more zero values
      [else
       (let ([options (find-options c 'solved)])
;         (if (for/or ([val (in-mutable-set options)])
         (if (for/or ([val (in-list options)])
               ;;(log (find-options c))
               ;;(log (format "(~a, ~a) ==> ~a" (cell-y c) (cell-x c) val))
               (set-cell-solved! c val)
               (solve))
             #t
             (begin
               ;;(log "i was wrong")
               (set-cell-solved! c 0)
               #f)))])))

(define (solve-game)
  (for ([c (filter(λ (c) (= (cell-solved c) 0)) (vector->list grid-of-cells))])
;    (when (= (set-count (find-options c)) 1)
    (when (= (length (find-options c 'solved)) 1)
;      (set-cell-solved! c (set-first (find-options c)))))
      (set-cell-solved! c (car (find-options c 'solved)))))

  (solve))

; -------------------
; util: test-functions
;--------------------

(define (print-options)
  (for ([index (in-range 81)])
     (let ([c (vector-ref grid-of-cells index)])
    (displayln (cell-options c)))))

(define (print-puzzle)
  (displayln "\n-----------------------")
  (for ([r (in-range 1 10)])
    (display (format "~a -> " r))
    (for ([c (in-range 1 10)])
      (display (format "~a " (cell-solved (get-cell r c)))))
    (displayln ""))
  (display "     ")
  (for ([c (in-range 1 10)]) (display (format "~a " "|")))
  (display "\n     ")
  (for ([c (in-range 1 10)]) (display (format "~a " c)))
  (displayln "\n-----------------------"))

(define (print-3grid)
  (displayln "static        play          solved")
  (for ([r (in-range 9)])
    (for ([c (in-range 9)])
      (display (cell-static (get-cell r c))))
    (display "  -  ")
    (for ([c (in-range 9)])
      (display (cell-play (get-cell r c))))
    (display "  -  ")
    (for ([c (in-range 9)])
      (display (cell-solved (get-cell r c))))
    (displayln "")))

(define (print-vector)
  (for ([index (in-range 81)])
    (let ([c (vector-ref grid-of-cells index)])
      (displayln (format "~a --> (~a, ~a, ~a) = ~a"
                         index (cell-y c) (cell-x c) (cell-z c) (cell-solved c))))))

;(init-data)
;(load-puzzle)
;(solve-game)
;; (for ([c (in-list (list-of-cells))])
;;       (displayln (format "(~a, ~a, ~a) = ~a"
;;                          (cell-y c) (cell-x c) (cell-z c) (cell-solved c))))
;(print-3grid)
