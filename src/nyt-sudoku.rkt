#lang racket

(require racket/gui/base)
(require net/url)
(require json)

(define (digit->char d)
  (integer->char (+ d 48)))

(define nyt-url "https://www.nytimes.com/puzzles/sudoku/medium")
(define curr-dir "/home/jvh/workspace/lisp/sudoku-puzzles/")

(define (write-to-file str curr-file)
  ;;  (let ([fname (put-file #f #f curr-dir curr-file)])
  (let ([fname (string-append curr-dir curr-file)])
    (if fname 
        (call-with-output-file fname
          (lambda (port) (displayln str port))
          #:exists 'truncate/replace)
        (error "no file selected!!"))))

(define (get-json-hash-table)
  (let* ([text  (port->string (get-pure-port (string->url  nyt-url)))]
         [raw-text (substring text
                              (cdr (car (regexp-match-positions "id=\"portal-game-modals\"" text)))
                              (cdr (car (regexp-match-positions "id=\"portal-editorial-content\"" text))))]
         [puzzle (substring raw-text
                            (cdr (car (regexp-match-positions "window.gameData = " raw-text)))
                            (cdr (car (regexp-match-positions "]}}}" raw-text))))]
         [json (with-input-from-string
                   puzzle
                 (λ () (read-json)))])
    json))

(define (output-filename ht level)
  (string-append
   "nyt-"
   (string-replace (hash-ref (hash-ref ht level) 'print_date) "-" "")
   "-"
   (symbol->string level)
   ".txt"))
  
(define (get-nyt-sudoku-puzzles)
  (let ([ht (get-json-hash-table)])
    (for ([level (list 'easy 'medium 'hard)])
      (write-to-file
       (list->string
        (map digit->char
             (hash-ref (hash-ref (hash-ref ht level) 'puzzle_data) 'puzzle)))
       (output-filename ht level))
      )))

  (get-nyt-sudoku-puzzles)
  