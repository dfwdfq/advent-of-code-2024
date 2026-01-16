#lang racket
(require racket/base)
(require racket/file)
(require racket/string)

(define raw-input (file->string "input"))
(define r (pregexp "mul\\((\\d+,\\d+)\\)"))
(define muls (regexp-match* r raw-input #:match-select car))

(define (prepare str)
  (let*
      ([no-mul (string-replace str "mul" "")]
       [no-lp  (string-replace no-mul "(" "")]
       [no-rp  (string-replace no-lp  ")" "")])
    (map string->number (string-split no-rp ","))))

(define (mult lst)
  (apply * lst))

(apply + (map mult (map prepare muls)))



