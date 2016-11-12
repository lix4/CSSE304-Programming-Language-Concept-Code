;;Xiwen Li
;;Assignment 11

;1(a)
(define-syntax my-let
	(syntax-rules ()
		[(_ ([var1 val1] ...) e1 e2 ...) 
		 ((lambda (var1 ...) e1 e2 ...) val1 ...)]
		[(_ init ([var1 val1] ...) e1 e2 ...)
		 (letrec ([init (lambda (var1 ...) e1 e2 ...)])
		 	(init val1 ...))]))

;;1(b)
(define-syntax my-or
	(syntax-rules ()
		[(_) #f]
		[(_ e1) e1]
		[(_ e1 e2)
		(let ([temp e1])
			 (if temp
			 	 temp
			 	 e2))]
		[(_ e1 e2 e3 ...)
		 (my-or (my-or e1 e2) e3 ...)]))
		 

;;1(c)
(define-syntax +=
	(syntax-rules ()
		[(_ e1 e2) (begin (set! e1 (+ e1 e2)) e1)]))

;;1(d)
(define-syntax return-first
	(syntax-rules ()
		[(_ e1) e1]
		[(_ e1 e2 ...) 
			(let ([temp e1])
				 (begin (return-first e2 ...) temp))]))


;;2
(define-datatype bintree bintree?
  	(leaf-node (datum number?))
  	(interior-node (key symbol?)
   				   (left-tree bintree?)
                   (right-tree bintree?)))

(define bintree-to-list
	(lambda (bt)
		(cases bintree bt
			[leaf-node (datum) (leaf-node datum)]
			[interior-node (key left-tree right-tree) 
			               (interior-node key left-tree right-tree)])))

;;3
(define max-interior
	(lambda (bt)
		(car (max-interior-rec bt))))

(define max-interior-rec
	(lambda (bt)
		(cases bintree bt
			[leaf-node (datum) (list '() datum datum)]
			[interior-node (key left-tree right-tree)
								 (let* ([left-tree-info (max-interior-rec left-tree)] 
								 	    [right-tree-info (max-interior-rec right-tree)] 
								    	[current-sum (+ (caddr left-tree-info) 
								  			  	        (caddr right-tree-info))])
											  (cond 
												    [(and (>= (cadr left-tree-info) (cadr right-tree-info)) (>= (cadr left-tree-info) current-sum)) 
												     (cond [(and (null? (car right-tree-info)) (null? (car left-tree-info)))
												     	    (list key (cadr left-tree-info) current-sum)]
												     	   [(and (null? (car left-tree-info)) (= (cadr left-tree-info) (cadr right-tree-info)))
												     	    (list (car right-tree-info) (cadr left-tree-info) current-sum)]
												     	   [(null? (car left-tree-info))
												     	    (list key (cadr left-tree-info) current-sum)]
												     	   [else (list (car left-tree-info) (cadr left-tree-info) current-sum)])]
												    [(equal? (max (cadr right-tree-info) current-sum (cadr left-tree-info)) current-sum) 
											  	     (list key current-sum current-sum)]
											  	    [(equal? (max (cadr right-tree-info) current-sum (cadr left-tree-info)) (cadr right-tree-info))
												      (if (null? (car right-tree-info))
												    	  (list key (cadr right-tree-info) current-sum)   
												          (list (car right-tree-info) (cadr right-tree-info) current-sum))]))])))