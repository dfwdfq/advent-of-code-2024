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


(define incorrect-updates
  (filter (lambda (x) (not (check-update x rules))) updates))


(define (build-graph update rules)
  (foldl (lambda (rule graph)
           (match rule
             [(list x y)
              (if (and (member x update) (member y update))
                  (hash-update graph x (lambda (neighbors) (cons y neighbors)) '())
                  graph)]))
         (make-immutable-hash (map (lambda (page) (cons page '())) update))
         rules))

(define (compute-in-degrees update graph)
  (let ([initial-degrees (make-immutable-hash (map (lambda (p) (cons p 0)) update))])
    (foldl (lambda (node deg-map)
             (foldl (lambda (neighbor deg-map)
                      (hash-update deg-map neighbor add1 0))
                    deg-map
                    (hash-ref graph node '())))
           initial-degrees
           update)))

(define (insert-in-order node queue original-pos)
  (let ([node-pos (hash-ref original-pos node)])
    (let loop ([q queue])
      (cond
        [(empty? q) (list node)]
        [(< node-pos (hash-ref original-pos (car q)))
         (cons node q)]
        [else (cons (car q) (loop (rest q)))]))))

(define (topological-sort update graph in-degree original-pos)
  (define (process queue result in-degree)
    (cond
      [(empty? queue) (reverse result)]
      [else
       (let* ([current (car queue)]
              [rest-queue (cdr queue)]
              [neighbors (hash-ref graph current '())]
              
              [new-in-degree
               (foldl (lambda (neighbor deg-map)
                        (hash-update deg-map neighbor sub1))
                      in-degree
                      neighbors)]
              
              [new-zero-nodes
               (filter (lambda (node) (zero? (hash-ref new-in-degree node)))
                       neighbors)]
              
              [new-queue
               (foldl (lambda (node q)
                        (insert-in-order node q original-pos))
                      rest-queue
                      new-zero-nodes)])
         
         (process new-queue (cons current result) new-in-degree))]))
  
  (let ([zero-nodes
         (sort (filter (lambda (node) (zero? (hash-ref in-degree node))) update)
               (lambda (a b)
                 (< (hash-ref original-pos a)
                    (hash-ref original-pos b))))])
    (process zero-nodes '() in-degree)))

(define (correct-update update rules)
  (let* ([original-pos (make-immutable-hash 
                        (map (lambda (page idx) (cons page idx))
                             update
                             (range (length update))))]
         [graph (build-graph update rules)]
         [in-degree (compute-in-degrees update graph)])
    (topological-sort update graph in-degree original-pos)))

(define part2-answer
  (let* ([corrected-updates (map (lambda (update) (correct-update update rules)) 
                                 incorrect-updates)]
         [middle-values (map middle corrected-updates)])
    (apply + middle-values)))


(display part2-answer)
