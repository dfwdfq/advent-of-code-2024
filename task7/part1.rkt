#lang racket

(require racket/file)
(require racket/string)
(require racket/list)

(define (set-eq str)
  (string-replace str ":" "="))

(define (clear str)
  (string-replace str "= " "="))

(define (prep-str str)
  (clear
   (set-eq str)))


(define raw-input
  (map
   prep-str (file->lines "input")))

(define (get-correct-range str)
  (range
   (-
    (string-length str) 1)))

(define (zip-pos-val str rng)
  (if (null? rng)
      '()
      (cons (list (string-ref str (car rng)) (car rng))
	    (zip-pos-val str (cdr rng)))))
(define (find-s-poses zipped)
  (if (null? zipped)
      '()
      (if (char=? (first (car zipped)) #\space)
	  (cons (second (car zipped)) (find-s-poses (cdr zipped)))
	  (find-s-poses (cdr zipped)))))

(define (zip a b)
  (apply map list (list a b)))

(define (permutations size elements)
  (if (zero? size)
      '(())
      (append-map (lambda (p)
                    (map (lambda (e)
                           (cons e p))
                         elements))
                  (permutations (sub1 size) elements))))

(define (string-insert str val k)
  (define result str)
  (string-set! result k val)
  (second (list '() result)))


(define (apply-all-insertions str ins)
  (if (null? ins)
      str
      (apply-all-insertions (string-insert str (first (first ins)) (second (first ins))) (cdr ins))))


(define (solve entry)
  (let* ([s-poses (find-s-poses (zip-pos-val entry (get-correct-range entry)))]
         [perm (permutations (length s-poses) '(#\+ #\*))]
         [insertions (map (lambda (x) (zip x s-poses)) perm)])
    (map (lambda (ins) (apply-all-insertions (string-copy entry) ins)) insertions)))

(define expressions
  (map solve raw-input))

(define (evaluate-infix str)
  (define len (string-length str))

  (define (parse-number i)
    (let skip-whitespace ((i i))
      (cond [(>= i len) (error "Unexpected end of string")]
            [(char-whitespace? (string-ref str i)) (skip-whitespace (+ i 1))]
            [else
             (let loop ((j i) (acc 0))
               (if (and (< j len) (char-numeric? (string-ref str j)))
                   (loop (+ j 1) (+ (* acc 10) (- (char->integer (string-ref str j)) 48)))
                   (values acc j)))])))

  (define (parse-operator i)
    (let skip-whitespace ((i i))
      (cond [(>= i len) (error "Unexpected end of string")]
            [(char-whitespace? (string-ref str i)) (skip-whitespace (+ i 1))]
            [else
             (let ((ch (string-ref str i)))
               (if (or (char=? ch #\+) (char=? ch #\*))
                   (values (string ch) (+ i 1))
                   (error "Expected operator, got" ch)))])))

  (define (eval-from result i)
    (if (>= i len)
        result
        (let-values ([(op i) (parse-operator i)])
          (let-values ([(next-num i) (parse-number i)])
            (let ((new-result (if (string=? op "+")
                                  (+ result next-num)
                                  (* result next-num))))
              (eval-from new-result i))))))

  (let-values ([(first-num i) (parse-number 0)])
    (eval-from first-num i)))

(define (extract-rval expr)
  (first
   (cdr
    (string-split expr "="))))

(define (extract-lval expr)
  (first
   (string-split expr "=")))

(define (apply-rval-extraction entry)
  (map extract-rval entry))

(define (compute entry)
  (let*
      ([lval (string->number (extract-lval (first entry)))]
       [rvals (apply-rval-extraction entry)]
       [results (map evaluate-infix rvals)])
    (filter
     (lambda (x) (= x lval)) results)))


(define (get-value entry)   
  (let ([lval (string->number (extract-lval (first entry)))])
    (if (not (null? (compute entry))) lval 0)))


(apply + (map get-value expressions))
