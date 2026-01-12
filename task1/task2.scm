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

(define S 0)
(for ([el left])
     (set! S
	   (+ S (* el (count (lambda (x) (= x el)) right)))))
(print S)
