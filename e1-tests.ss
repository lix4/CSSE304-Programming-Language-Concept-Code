;; Test code for CSSE 304 Exam 1 201710

(define (test-range-of-numbers)
    (let ([correct '(
		     0 1 5 0 12
		     )]
          [answers 
            (list 
	     (range-of-numbers '(4))
	     (range-of-numbers '(4 5))
	     (range-of-numbers '(4 2 -1))
	     (range-of-numbers '(4 4))
	     (range-of-numbers '(4 -2 7 -1 3 10 6 8 -2 3 8))
	     )])
      (display-results correct answers equal?)))

(define (test-symmetric?)
    (let ([correct '(
		     #t #f #t #t #f #t
		     )]
          [answers 
            (list 
	     (symmetric? '((a b) (b c) (a a) (c b) (b b) (b a)))
	     (symmetric? '((a b) (b c) (a a) (b b) (b a)))
	     (symmetric? '((a a)))	      
	     (symmetric? '((a b)(b a) (a a)))
	     (symmetric? '((a b)))
	     (symmetric? '())
	     )])
      (display-results correct answers equal?)))

(define (test-los->ms)
    (let ([correct '(
		     ((b 1)(a 2))
		     ((a 7) (b 3) (c 2) (d 1) (e 1))
		     )]
          [answers 
            (list 
	     (los->ms '(a b a)) 
	     (los->ms '(a a b c b b a d a a a a c e))
	     )])
      (display-results correct answers  sequal?-grading)))

(define (test-symbols-with-n-count)
    (let ([correct '(
		     2 1 4 0 0 
		     )]
          [answers 
            (list 
	     (symbols-with-n-count '(a a b c b b a d a a a a c e) 1)
	     (symbols-with-n-count '(a a b c b b a d a a a a c e) 7)
	     (symbols-with-n-count '(f a d a b c b b a d f a h a a a g c e f c d) 3) ; b c e f 
	     (symbols-with-n-count '(a a b c b b a d a a a a c e) 4)
	     (symbols-with-n-count '() 1)
	     )])
      (display-results correct answers equal?)))


;-----------------------------------------------

 (define display-results
  (lambda (correct results test-procedure?)
     (display ": ")
     (pretty-print 
      (if (andmap test-procedure? correct results)
          'All-correct
          `(correct: ,correct yours: ,results)))))


(define sequal?-grading
  (lambda (l1 l2)
    (cond
     ((null? l1) (null? l2))
     ((null? l2) (null? l1))
     ((or (not (set?-grading l1))
          (not (set?-grading l2)))
      #f)
     ((member (car l1) l2) (sequal?-grading
                            (cdr l1)
                            (rember-grading
                             (car l1)
                             l2)))
     (else #f))))

(define set?-grading
  (lambda (s)
    (cond [(null? s) #t]
          [(not (list? s)) #f]
          [(member (car s) (cdr s)) #f]
          [else (set?-grading (cdr s))])))

(define rember-grading
  (lambda (a ls)
    (cond
     ((null? ls) ls)
     ((equal? a (car ls)) (cdr ls))
     (else (cons (car ls) (rember-grading a (cdr ls)))))))

(define set-equals? sequal?-grading)

(define find-edges  ; e know that this node is in the graph before we do the call
  (lambda (graph node)
    (let loop ([graph graph])
      (if (eq? (caar graph) node)
	  (cadar graph)
	  (loop (cdr graph))))))

;; Problem 8  graph?
(define set?  ;; Is this list a set?  If not, it is not a graph.
  (lambda (list)
    (if (null? list) ;; it's an empty set.
	#t
	(if (member (car list) (cdr list))
	    #f
	    (set? (cdr list))))))


(define graph?
  (lambda (obj)
    (and (list? obj)
	 (let ([syms (map car obj)])
	   (and (set? syms)
		(andmap symbol? syms)
		(andmap (lambda (x)
			  (andmap (lambda (y) (member y (remove (car x) syms)))
				  (cadr x)))
			obj))))))
    
(define graph-equal?
  (lambda (a b)
    (and
     (graph? a) 
     (graph? b)
     (let ([a-nodes (map car a)]
	   [b-nodes (map car b)])
       (and 
	(set-equals? a-nodes b-nodes)
	    ; Now  See if the edges from each node are equivalent in the two graphs.
	(let loop ([a-nodes a-nodes])
	  (if (null? a-nodes)
	      #t
	      (let ([a-edges (find-edges a (car a-nodes))]
		    [b-edges (find-edges b (car a-nodes))])
		(and (set-equals? a-edges b-edges)
		     (loop (cdr a-nodes)))))))))))

(define (test-graph-equal)
  (list
   (graph-equal? '((a (b)) (b (a))) '((b (a)) (a (b))))
   (graph-equal? '((a (b c d)) (b (a c d)) (c (a b d)) (d (a b c)))
		 '((b (a c d)) (c (a b d)) (a (b d c)) (d (b a c))))
   (graph-equal? '((a ())) '((a ())))
   (graph-equal? '((a (b c)) (b (a c)) (c (a b))) '((a (b c)) (b (a c)) (c (a b))))
   (graph-equal? '() '())
   ))



(define g test-graph-equal)
	   
	  
     



;You can run the tests individually, or run them all
;#by loading this file (and your solution) and typing (r)

(define (run-all)
  (display 'range-of-numbers) 
  (test-range-of-numbers )
  (display 'symmetric?) 
  (test-symmetric?)
  (display 'los->ms) 
  (test-los->ms)
  (display 'symbols-with-n-count) 
  (test-symbols-with-n-count)    
)

(define r run-all)

