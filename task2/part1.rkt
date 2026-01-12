#lang racket
(require racket/file)
(require racket/base)
(require racket/string)
(require racket/list)

(define raw-input (file->lines "input"))

(define (split-by-space str)
  (string-split str " "))

(define (conv-to-number lst)
  (map string->number lst))

(define (conv-line line)
  (conv-to-number (split-by-space line)))

(define (list->pairs lst)
  (for/list ([current lst]
             [next (cdr lst)])
	    (list current next)))

(define (is-incr-helper? lst)
  (for/list ([current lst]
	     [next (cdr lst)])
	    (> next current)))
(define (is-incr? lst)
  (if (null? lst)
      #t
      (and (car lst) (is-incr? (cdr lst)))))

(define (is-decr-helper? lst)
  (for/list ([current lst]
	     [next (cdr lst)])
	    (< next current)))
(define (is-decr? lst)
  (if (null? lst)
      #t
      (and (car lst) (is-decr? (cdr lst)))))

(define (is-order-correct? lst)
  (xor (is-incr? (is-incr-helper? lst))
      (is-decr? (is-decr-helper? lst))))
  


(define (dist pair)
  (let ([fst (first pair)]
	[snd (second pair)])
    (abs (- fst snd))))

(define (list-dist lst)
  (map dist lst))

(define (is-ok? d)
  (if (member d '(1 2 3))
      #t
      #f))

(define (is-dist-ok? lst)
  (if (null? lst)
      #t
      (and (car lst) (is-dist-ok? (cdr lst)))))



(define (check-dist lst)
  (is-dist-ok?
   (map is-ok?
	(list-dist
	 (list->pairs lst)))))

(define (is-right? lst)
  (and (check-dist lst)
       (is-order-correct? lst)))

(define input (map conv-line raw-input))
(print (count is-right? input))
