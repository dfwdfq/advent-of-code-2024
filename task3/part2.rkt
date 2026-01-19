#lang racket
(require racket/base)
(require racket/file)
(require racket/string)

(define raw-input (file->string "input"))

(define r (pregexp "mul\\(\\d+,\\d+\\)|don't\\(\\)|do\\(\\)"))
(define tokens (regexp-match* r raw-input))

(define (prepare str)
  (let*
      ([no-mul (string-replace str "mul" "")]
       [no-lp  (string-replace no-mul "(" "")]
       [no-rp  (string-replace no-lp  ")" "")])
    (map string->number (string-split no-rp ","))))

(define (compute token flag?)
  (if (eq? flag? #t)
      (apply *
	     (prepare token))
      0))

(define (process  lst flag?)
  (if (null? lst)
      '()
      (cond
       [(string=? (car lst) "do()")
	(process (cdr lst) #t)]
       [(string=? (car lst) "don't()")
	(process (cdr lst) #f)]
       [else (cons (compute (car lst) flag?)
		   (process (cdr lst) flag?))])))

(apply + (process tokens #t))

