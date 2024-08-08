#lang racket

(provide *log-active*
         log
         split-in-chunks
         index->mini-grid
         char->digit
         digit->char)

(define *log-active* #f)

(define (log message)
  (when *log-active*
    (displayln message)))

(define (zip a b)
  (foldl (lambda (x y result) (append result (list (cons x y))))
         '() a b))

(define (split-in-chunks n lst)
  (cond
    [(null? lst) '()]
    [(< (length lst ) n) (cons lst (split-in-chunks n '()))]
    [else
     (let ((first-chunk (take lst n))
           (rest (drop lst n)))
       (cons first-chunk (split-in-chunks n rest)))]))

(define (char->digit c)
  (if (char? c)
      (let ([val (- (char->integer c) 48)])
        (unless  (and (>= val 0) (<= val 9))
          (error "invalid value in puzzle-string"))
        val)
      c))

(define (digit->char d)
  (integer->char (+ d 48)))

(define (string->vector puzzle)
  (let ([p (string-replace puzzle " " "")])
    (unless (= (string-length p) 81)
      (error "length of sudoku-puzzle is not 81"))
    (build-vector 81 (lambda (i) (char->digit (string-ref p i))))))

(define (vector->string v) ; Vector -> String
  (let ([char-list '()])
    (for ([d v])
      (set! char-list (cons (digit->char d) char-list)))
    (list->string (reverse char-list))))

; -------------------
; tables
;--------------------

; Natural (0..80) -> Natural (1..9)
; interp. match index in grid-of-cells (vector) to
; one of the 9 sudoku mini-grids
(define (index->mini-grid n)
    (+ (add1 (* (quotient (quotient n 9) 3) 3)) (quotient (remainder n 9) 3)))

(define mini-grid->index-table
 (let ([sum '()])
   (for-each (lambda (x) (set! sum (cons (cdr x) sum)))
             (sort
              (foldl (lambda (x y result) (append result (list (cons x y))))
                     '()
                     (map index->mini-grid (build-list 81 values))
                     (build-list 81 values))
              (lambda (x y)(< (car x) (car y)))))
   (list->vector (split-in-chunks 9 (reverse sum)))))

(define column->index-table
 (let-values ([(a b c d e f g h i)
               (apply values (split-in-chunks 9 (build-list 81 values)))])
   (list->vector (map (lambda a-rest a-rest) a b c d e f g h i))))

(define row->index-table
 (list->vector (split-in-chunks 9 (build-list 81 values))))
