#lang racket
(require racket/file)
(require racket/list)

(define raw-input (string-split (file->string "input") "\n"))

(define (is-correct str)
  (not (string=? str "")))

(define (conv-input rule-str sep)
  (map
   string->number
   (string-split rule-str sep)))


(define rules
  (map
   (lambda (x) (conv-input x "|"))
   (takef raw-input is-correct)))

(define updates
  (map
   (lambda (x) (conv-input x ","))
     (takef (reverse raw-input) is-correct)))


(define (build-pos-map update start)
  (if (= start (length update))
      '()
      (cons
       (list (list-ref update start)
	     (index-of update (list-ref update start)))
	    (build-pos-map update (+ 1 start)))))
(define (get-pos-map-keys pos-map)
  (map car pos-map))
(define (get-val-by-key pos-map key)
  (second
   (car
    (filter (lambda (x) (= (car x) key)) pos-map))))

(define (in? el lst)
  (let
      ([res (filter (lambda (x) (= x el)) lst)])
    (if (null? res)
	#f
	#t)))


(define (is-rule-in-pos-map rule pos-map)
  (let
      ([X (first rule)]
       [Y (second rule)]
       [keys (get-pos-map-keys pos-map)])
    (and (in? X keys)
	 (in? Y keys))))

(define (check-rule rule pos-map)
  (let*
      ([X (first rule)]
       [Y (second rule)]
       [X_pos (get-val-by-key  pos-map X)]
       [Y_pos (get-val-by-key  pos-map Y)])
    (< X_pos Y_pos)))
       
(define (check-update update rules)
  (let
      ([pos-map (build-pos-map update 0)])
    (if (null? rules)
	#t
	(if (is-rule-in-pos-map (car rules) pos-map)
	    (if (check-rule (car rules) pos-map)
		(check-update update (cdr rules))
		#f)
	    (check-update update (cdr rules))))))

(define (middle lst)
  (let
      ([i (floor (/ (length lst) 2))])
    (list-ref lst i)))

(apply +
       (map
	middle
	(filter
	 (lambda (x) (check-update x rules)) updates)))





