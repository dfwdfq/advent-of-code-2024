#lang racket

(require racket/file)
(require racket/list)
(require racket/set)

(define raw-input (string-split (file->string "input") "\n"))
(define width  (string-length (car raw-input)))
(define height (length raw-input))

(define (state-in? state state-set)
  (set-member? state-set state))

(define (swap-coords pos) (list (second pos) (first pos)))

(define (find-char-in-line line pos char)
  (if (null? line)
      '()
      (if (char=? (car line) char)
          (cons pos (find-char-in-line (cdr line) (+ pos 1) char))       
          (find-char-in-line (cdr line) (+ pos 1) char))))

(define (append-column-n lst n)
  (map (lambda (x) (list x n)) lst))

(define (process-line line char n)
  (append-column-n (find-char-in-line (string->list line) 0 char) n))

(define (find-all-chars mat char n)
  (if (null? mat)
      '()
      (cons (process-line (car mat) char n)
            (find-all-chars (cdr mat) char (+ n 1)))))

(define obstacle-coords 
  (find-all-chars raw-input #\# 0))

(define original-obstacles 
  (list->set (map (lambda (coord) (swap-coords coord)) 
                  (apply append obstacle-coords))))

(define start-raw 
  (car (filter (lambda (x) (not (null? x)))
               (find-all-chars raw-input #\^ 0))))

(define start (swap-coords (car start-raw)))

(define up-dir '(-1 0))
(define down-dir '(1 0))
(define left-dir '(0 -1))
(define right-dir '(0 1))

(define (turn-right dir)
  (cond
    [(equal? dir up-dir) right-dir]
    [(equal? dir right-dir) down-dir]
    [(equal? dir down-dir) left-dir]
    [(equal? dir left-dir) up-dir]))

(define (vec-add vec1 vec2)
  (list (+ (first vec1) (first vec2))
        (+ (second vec1) (second vec2))))

(define (is-out? pos)
  (let ([row (first pos)]
        [col (second pos)])
    (or (< row 0) (>= row height)
        (< col 0) (>= col width))))

(define (simulate-with-extra-obstacle extra-obstacle)
  (define obstacles (set-add original-obstacles extra-obstacle))
  
  (define (is-obstacle? pos)
    (set-member? obstacles pos))
  
  (let loop ([visited-states (set)]
             [position start]
             [direction up-dir])
    (define current-state (list position direction))
    
    (cond
      [(state-in? current-state visited-states)
       #t]
      
      [(is-out? position)
       #f]
      
      [else
       (define next-pos (vec-add position direction))
       
       (cond
         [(is-out? next-pos)
          (loop (set-add visited-states current-state)
                next-pos
                direction)]
         
         [(is-obstacle? next-pos)
          (loop (set-add visited-states current-state)
                position
                (turn-right direction))]
         
         [else
          (loop (set-add visited-states current-state)
                next-pos
                direction)])])))

(define (find-loop-positions)
  (define loop-positions '())
  
  (for ([row (range height)])
    (for ([col (range width)])
      (define pos (list row col))
      
      (when (and (not (equal? pos start))
                 (not (set-member? original-obstacles pos)))
        
        (when (simulate-with-extra-obstacle pos)
          (set! loop-positions (cons pos loop-positions))))))
  
  loop-positions)

(define loop-positions (find-loop-positions))
(length loop-positions)
