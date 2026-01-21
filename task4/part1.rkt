#lang racket
(require racket/base)
(require racket/file)
(require racket/string)

(define input (string-split (file->string "input") "\n"))
(define line-len (string-length (car input)))
(define height   (length input))

(define (find-x-in-line line pos)
  (if (null? line)
      '()
      (if (char=? (car line) #\X)
          (cons pos
                (find-x-in-line (cdr line) (+ pos 1)))       
          (find-x-in-line (cdr line) (+ pos 1)))))

(define (append-column-n lst n)
  (map (lambda (x) (list x n)) lst))

(define (process-line line n)
  (append-column-n
   (find-x-in-line
    (string->list line) 0) n))

(define (find-all-x mat n)
  (if (null? mat)
      '()
      (cons (process-line (car mat) n)
            (find-all-x (cdr mat) (+ n 1)))))

(define x-poses (apply append (find-all-x input 0)))

(define (grid-ref grid row col)
  (string-ref (list-ref grid row) col))
(define (get-element grid pos)
  (grid-ref grid (second pos) (first pos)))

(define dirs '(
               (-1 0)   ;left
               (1 0)    ;right
               (0 -1)   ;up
               (0 1)    ;down
               (-1 -1)  ;up-left
               (1 -1)   ;up-right
               (-1 1)   ;down-left
               (1 1)    ;down-right
               ))

(define (vec-add vec1 vec2)
  (list
   (+ (first vec1) (first vec2))
   (+ (second vec1) (second vec2))))

(define (is-in? pos)
  (let
      ([x (first pos)]
       [y (second pos)])
    (and
     (>= x 0)
     (< x line-len)
     (>= y 0)
     (< y height))))

(define (compute-substr-xy pos dir)
  (let* ([vec0 pos] 
         [vec1 (vec-add vec0 dir)]
         [vec2 (vec-add vec1 dir)]
         [vec3 (vec-add vec2 dir)])
    (if (and
         (is-in? vec0)
         (is-in? vec1)
         (is-in? vec2)
         (is-in? vec3))
        (list vec0 vec1 vec2 vec3)
        '())))

(define (extract-substr poses grid)
  (if (null? poses)
      ""
      (apply string (map (lambda (x) (get-element grid x)) poses))))

(define (extract-substr-in-dir dir grid)
  (map
   (lambda (x)
     (extract-substr
      (compute-substr-xy x dir) grid)) x-poses))

(define substrings
  (apply append (map (lambda (x)
                       (extract-substr-in-dir x input)) dirs)))

(define (reverse-str str)
   (list->string (reverse (string->list str))))


(+ (count (lambda (x) (string=? x "XMAS")) substrings)
   (count (lambda (x) (string=? (reverse-str x) "XMAS")) substrings))
    
