(define (height m) (vector-length m))
(define (width m) (string-length (vector-ref m 0)))
(define (in-bounds? m x y) (and (>= x 0) (>= y 0) (< y (height m)) (< x (width m))))
(define (at m x y) (string-ref (vector-ref m y) x))
(define (cell-empty? m x y) (not (equal? (at m x y) #\#)))

(define (shift-valid? m x y nx ny)
  (let ([c (at m x y)])
    (cond
      [(equal? part "2") #t]
      [(equal? c #\.) #t]
      [(and (equal? c #\>) (= y ny) (= nx (+ 1 x))) #t]
      [(and (equal? c #\<) (= y ny) (= nx (- 1 x))) #t]
      [(and (equal? c #\^) (= ny (- 1 y)) (= x nx)) #t]
      [(and (equal? c #\v) (= ny (+ 1 y)) (= x nx)) #t]
      [else #f])))

(define (cell-neighbours m x y)
  (for/fold ([acc '()]) ([dx '(0 0 -1 1)] [dy '(1 -1 0 0)])
    (let ([nx (+ x dx)] [ny (+ y dy)])
      (if (and (in-bounds? m nx ny) (cell-empty? m nx ny) (shift-valid? m x y nx ny))
        (cons (cons nx ny) acc)
        acc))))

(define (intersection? m x y)
  (and (cell-empty? m x y) (> (length (cell-neighbours m x y)) 2)))

(define (intersections m)
  (for*/fold ([acc '()]) ([x (in-range (width m))] [y (in-range (height m))])
    (if (intersection? m x y) (cons (cons x y) acc) acc)))

(define (graph-nodes m)
  (let ([start (cons 1 0)]
        [end (cons (- (width m) 2) (- (height m) 1))])
    (list->set (cons start (cons end (intersections m))))))

(define (walk-path m g x y p)
  (if (set-member? g (cons x y))
    (cons (cons x y) 1)
    (let ([neighbours (filter (lambda (n) (not (equal? n p))) (cell-neighbours m x y))])
      (if (null? neighbours) null
        (let* ([next (car neighbours)]
               [fin (walk-path m g (car next) (cdr next) (cons x y))])
          (if (null? fin) null (cons (car fin) (+ 1 (cdr fin)))))))))

(define (graph-neighbours m g x y)
  (for/fold ([acc '()]) ([n (cell-neighbours m x y)])
    (let ([path (walk-path m g (car n) (cdr n) (cons x y))])
      (if (null? path) acc (cons path acc)))))

(define (map->graph m)
  (let ([g (graph-nodes m)])
    (for/hash ([node g])
      (values node (graph-neighbours m g (car node) (cdr node))))))

(define (walk-graph g p x y ty seen)
  (if (equal? y ty) 0
    (for/fold ([acc null]) ([np (hash-ref g (cons x y))])
      (let ([n (car np)] [dist (cdr np)])
        (if (or (equal? n p) (set-member? seen n)) acc
          (let ([d (walk-graph g (cons x y) (car n) (cdr n) ty (set-add seen (cons x y)))])
            (cond
              [(null? d) acc]
              [(null? acc) (+ d dist)]
              [(> (+ d dist) acc) (+ d dist)]
              [else acc])))))))

(define args (current-command-line-arguments))
(define task (vector-ref args 0))
(define part (vector-ref args 1))
(define m (list->vector (file->lines (string-append "in/" task part ".txt"))))
(define g (map->graph m))
(println (walk-graph g null 1 0 (- (height m) 1) (set)))
