;; Test code for CSSE 304 Assignment 8

(define (test-slist-map)
    (let ([correct '(
		     ()
		     (#t #t (#t) () #t)
		     ((a x) (b x) ((c x)) () (d x))
		     ((bb (cc) dd) ee ((aa)) () ee)
		     )]
          [answers 
            (list 
	     (slist-map symbol? '())
	     (slist-map symbol? '(a b (c) () d))
	     (slist-map (lambda (x) (list x 'x)) '(a b (c) () d))
	     (slist-map (lambda (x) 
			  (let ([s (symbol->string x)]) 
			    (string->symbol(string-append s s)))) 
			'((b (c) d) e ((a)) () e))
	     )])
      (display-results correct answers equal?)))

(define (test-slist-reverse)
    (let ([correct '(
		     ()
		     (b a)
		     (c (b a))
		     (f () (e (d)) c (b a))
		     )]
          [answers 
            (list 
	     (slist-reverse '())
	     (slist-reverse '(a b))
	     (slist-reverse '((a b) c))
	     (slist-reverse '((a b) c ((d) e) () f))
	     )])
      (display-results correct answers equal?)))

(define (test-slist-paren-count)
    (let ([correct '(
		     2
		     4
		     8
		     10
		     10
		     )]
          [answers 
            (list 
	     (slist-paren-count '(a))
	     (slist-paren-count '((a)))
	     (slist-paren-count '((a ((b)))))
	     (slist-paren-count '((a ((b) ()))))
	     (slist-paren-count '((a ((b c d e) () f))))
	     )])
      (display-results correct answers equal?)))

(define (test-slist-depth)
    (let ([correct '(
		     1
		     1
		     2
		     5
		     4
		     4
		     )]
          [answers 
            (list 
	     (slist-depth '())
	     (slist-depth '(a))
	     (slist-depth '((a)))
	     (slist-depth '(() (((())))))
	     (slist-depth '( () (a) ((s b (c) ()))))
	     (slist-depth '(a (b c (d (x x) e)) ((f () g h))))
	     )])
      (display-results correct answers equal?)))

(define (test-slist-symbols-at-depth)
    (let ([correct '(		    
		     (b c)
		     (a d)
		     ()
		     (a i)
		     (b c)
		     (d e f g h)
		     (x x)
		     )]
          [answers 
            (list 
	     (slist-symbols-at-depth '(a (b c) d) 2)
	     (slist-symbols-at-depth '(a (b c) d) 1)
	     (slist-symbols-at-depth '(a (b c) d) 3)
	     (slist-symbols-at-depth '(a (b c (d (x x) e)) ((f () g h)) i) 1)
	     (slist-symbols-at-depth '(a (b c (d (x x) e)) ((f () g h)) i) 2)
	     (slist-symbols-at-depth '(a (b c (d (x x) e)) ((f () g h)) i) 3)
	     (slist-symbols-at-depth '(a (b c (d (x x) e)) ((f () g h)) i) 4)
	     )])
      (display-results correct answers equal?)))

(define (test-group-by-two)
    (let ([correct '(		    
		     ()
		     ((a))
		     ((a b))
		     ((a b)(c))
		     ((a b) (c d) (e f) (g))
		     ((a b) (c d) (e f) (g h))
		     )]
          [answers 
            (list 
	     (group-by-two '())
	     (group-by-two '(a))
	     (group-by-two '(a b))
	     (group-by-two '(a b c))
	     (group-by-two '(a b c d e f g))
	     (group-by-two '(a b c d e f g h))
	     )])
      (display-results correct answers equal?)))

(define (test-group-by-n)
    (let ([correct '(		    
		     ()
		     ((a b c) (d e f) (g))
		     ((a b c d) (e f g))
		     ((a b c d) (e f g h))
		     ((a b c d e f g) (h i j k l m n) (o))
		     ((a b c d e f g h))
		     ((a b c d e f g h i j k l m n o p q) (r s t))
		     )]
          [answers 
            (list 
	     (group-by-n '() 3)
	     (group-by-n '(a b c d e f g) 3)
	     (group-by-n '(a b c d e f g) 4)
	     (group-by-n '(a b c d e f g h) 4)
	     (group-by-n '(a b c d e f g h i j k l m n o) 7)
	     (group-by-n '(a b c d e f g h) 17)
	     (group-by-n '(a b c d e f g h i j k l m n o p q r s t) 17)
	     )])
      (display-results correct answers equal?)))


(define (test-subst-leftmost)
    (let ([correct '(
		     ()
		     (k b)
		     (a k a b)
		     (a ((k b)) a b)
		     ((c d a (e () f k (c b)) (a b)) (b))
		     (c (b e) a d)
		     )]
          [answers 
            (list 
	     (subst-leftmost 'k 'b '() eq?)
	     (subst-leftmost 'k 'b '(b b) eq?)
	     (subst-leftmost 'k 'b '(a b a b) eq?)
	     (subst-leftmost 'k 'b '(a ((b b)) a b) eq?)
	     (subst-leftmost 'k 'b '((c d a (e () f b (c b)) (a b)) (b)) eq?)
	     (subst-leftmost 'b 'a '(c (A e) a d) 
			     (lambda (x y) 
			       (string-ci=? 
				(symbol->string x) 
				(symbol->string y))))
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
  ; (display 'slist-map ) 
  ; (test-slist-map )
  ; (display 'slist-reverse) 
  ; (test-slist-reverse)
  ; (display 'slist-paren-count) 
  ; (test-slist-paren-count)
  ; (display 'slist-depth) 
  ; (test-slist-depth)    
  ; (display 'slist-symbols-at-depth) 
  ; (test-slist-symbols-at-depth)
  ; (display 'group-by-two)
  ; (test-group-by-two)
  ; (display 'group-by-n)
  ; (test-group-by-n)
  (display 'subst-leftmost)
  (test-subst-leftmost)  

)

(define r run-all)

