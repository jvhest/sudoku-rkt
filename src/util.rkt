#lang racket

(provide load-puzzle
         locate-value
         update-value
         log
         print-puzzle
         solved?)

(define *log-active* #f)

(define (log message)
  (when *log-active*
    (displayln message)))

(define (load-puzzle puzzle)
  (let ([board-data (make-vector 81 0)])
  (unless (= (string-length puzzle) 81)
    (error "length of puzzle is not 81"))
  (for ([i 81])
    (vector-set! board-data i (- (char->integer (string-ref puzzle i)) 48)))
  board-data))

(define (locate-value puzzle row col)
  (vector-ref puzzle (+ (* (sub1 row) 9) (sub1 col))))

(define (update-value puzzle row col val)
  (vector-set! puzzle (+ (* (sub1 row) 9) (sub1 col)) val))

;; returns #t if the puzzle is solved, otherwise returns #f
(define (solved? puzzle)
  (not (vector-member 0 puzzle)))

(define (print-puzzle puzzle)
  (displayln "\n-----------------------")
  (for ([r (in-range 1 10)])
    (display (format "~a -> " r))
    (for ([c (in-range 1 10)])
      (display (format "~a " (locate-value puzzle r c))))
    (displayln ""))
  (display "     ")
  (for ([c (in-range 1 10)]) (display (format "~a " "|")))
  (display "\n     ")
  (for ([c (in-range 1 10)]) (display (format "~a " c)))
  (displayln "\n-----------------------"))

(define (valid-move-column puzzle col val)
  (let ([valid #t])
    (for ([row (in-range 1 10)])
          (when (equal? (locate-value puzzle row col)) val)
            (set! valid #f))
    valid))

(define (valid-move-row puzzle row val)
  (let ([valid #t])
    (for ([col (in-range 1 10)])
      (when (equal? (locate-value puzzle row col)) val)
        (set valid #f))
    valid))

(define (valid-move-square puzzle row col val)
  (let ([x0 (+ (* (floor (/ (- col 1) 3)) 3) 1)]
        [y0 (+ (* (floor (/ (- row 1) 3)) 3) 1)]
        [valid #t])
  (for ([i 3])
    (for ([j 3])
     ;; (print (format "row ~a col ~a -> (~a,~a) => ~a" row col y0 x0 (offset-in-board-data (+ y0 i) (+ x0 j))))
      (when (equal? (locate-value puzzle (+ y0 i) (+ x0 j))) val)
        (set! valid #f)))
  valid))

(define (valid-move puzzle row col val)
  (and (valid-move-row puzzle row val)
       (valid-move-column puzzle col val)
       (valid-move-square puzzle row col val)))
