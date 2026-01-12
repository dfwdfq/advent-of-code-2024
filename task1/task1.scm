#lang racket
(require racket/file)
(require racket/string)
(require racket/list)
(require racket/base)
(require math/base)

(define raw-input (file->lines "input"))

(define (split-by-space str)
  (string-split str " "))

(define (conv-to-number lst)
  (let ([fst (car lst)]
       [snd (car (cdddr lst))])
  (map string->number (list fst snd))))

(define (conv-line-to-pair line)
  (conv-to-number (split-by-space line)))

(define input (map conv-line-to-pair raw-input))

(define left (sort (map first input) <))
(define right (sort (map second input) <))

(define (make-pairs lst1 lst2)
  (if (null? lst1) '()
      (cons (list (car lst1) (car lst2))
	    (make-pairs (cdr lst1) (cdr lst2)))))


(define (dist lst)
  (abs (- (first lst) (second lst))))

(sum (map dist (make-pairs left right)))
