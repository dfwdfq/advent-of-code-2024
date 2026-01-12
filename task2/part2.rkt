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
  (or (is-incr? (is-incr-helper? lst))
      (is-decr? (is-decr-helper? lst))))

(define (dist pair)
  (let ([fst (first pair)]
        [snd (second pair)])
    (abs (- fst snd))))

(define (list-dist lst)
  (map dist lst))

(define (is-ok? d)
  (<= d 3))

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

(define (remove-at lst i)
  (append (take lst i) (drop lst (+ i 1))))

(define (exists-removal-safe? lst)
  (for/or ([i (in-range (length lst))])
    (is-right? (remove-at lst i))))

(define (safe-with-dampener? lst)
  (or (is-right? lst)
      (exists-removal-safe? lst)))

(define input (map conv-line raw-input))

(print (count safe-with-dampener? input))
