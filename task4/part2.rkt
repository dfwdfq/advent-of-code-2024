#lang racket
(require racket/base)
(require racket/file)
(require racket/string)

(define input (string-split (file->string "input") "\n"))
(define line-len (string-length (car input)))
(define height   (length input))

(define (find-a-in-line line pos)
  (if (null? line)
      '()
      (if (char=? (car line) #\A)
          (cons pos
                (find-a-in-line (cdr line) (+ pos 1)))       
          (find-a-in-line (cdr line) (+ pos 1)))))

(define (append-column-n lst n)
  (map (lambda (x) (list x n)) lst))

(define (process-line line n)
  (append-column-n
   (find-a-in-line
    (string->list line) 0) n))

(define (find-all-a mat n)
  (if (null? mat)
      '()
      (cons (process-line (car mat) n)
            (find-all-a (cdr mat) (+ n 1)))))

(define a-poses (apply append (find-all-a input 0)))

(define (grid-ref grid row col)
  (string-ref (list-ref grid row) col))
(define (get-element grid pos)
  (grid-ref grid (second pos) (first pos)))

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


(define (valid? pos)
  (let
      ([center pos]
       [pos1 (vec-add pos '(-1 -1))]
       [pos2 (vec-add pos '(1 1))]
       [pos3 (vec-add pos '(-1 1))]
       [pos4 (vec-add pos '(1 -1))])
    (and (is-in? center)
	 (is-in? pos1)
	 (is-in? pos2)
	 (is-in? pos3)
	 (is-in? pos4))))

(define valid-a-poses (filter valid? a-poses))

(define (extract-substr pos)
  (let
      ([center pos]
       [pos1 (vec-add pos '(-1 -1))]
       [pos2 (vec-add pos '(1 1))]
       [pos3 (vec-add pos '(-1 1))]
       [pos4 (vec-add pos '(1 -1))])
    (list
     (list (get-element input pos4)
	   (get-element input center)
	   (get-element input pos3))
     (list (get-element input pos2)
	   (get-element input center)
	   (get-element input pos1)))))

(define (to-string chars)
  (apply string chars))

(define substrings
  (map extract-substr valid-a-poses))

(define (conv-subs subs)
  (if (null? subs)
      '()
      (cons (map to-string (car subs))
	    (conv-subs (cdr subs)))))

(define correct-subs (conv-subs substrings))

(define (is-correct? lst)
  (let*
      ([left (first lst)]
       [right (second lst)]
       [c1 (or (string=? left "MAS")
	       (string=? left "SAM"))]
       [c2 (or (string=? right "MAS")
	       (string=? right "SAM"))])
    (and c1 c2)))

(count is-correct? correct-subs)
