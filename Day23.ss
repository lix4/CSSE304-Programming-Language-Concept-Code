(define apply-continuation
	(lambda (k v)
		(k v)))

(define fact-cps
	(lambda fact-cps (n k)
		(if (zero? n)
			(apply-continuation k 1)
			(fact-cps (- n 1) 
				      (lambda (v) 
				      	      (apply-continuation k (* n v)))))))

(define list-copy-cps
	(lambda (L k)
		(if (null? L)
			(apply-continuation k '())
			(list-copy-cps (cdr L)
				           (lambda (copied-cdr)
				           	       (apply-continuation k 
				           	       	                   (cons (car L) copied-cdr)))))))

(define memq-cps
	(lambda (sym ls k)
		(cond [(null? ls) (apply-continuation k #f)]
			  [(eq? (car ls ) sym) (apply-continuation k #t)]
			  [else (memq-cps sym (cdr ls) k)])))

(define intersection-cps
	(lambda (los1 los2 k)
		(if (null? los1)
			(apply-continuation k '())
			(intersection-cps (cdr los1) los2
				              (lambda (intersection-with-cdr)
				              	      (memq-cps (car los1) los2
				              	      	        (lambda (car-in-los2)
				              	      	        	    (apply-continuation k
				              	      	        	    	                (if car-in-los2
				              	      	        	    	                    (cons (car los1) intersection-with-cdr)
				              	      	        	    	                    intersection-with-cdr)))))))))

(intersection-cps '(a v x) '(a b c) list)

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define union-cps
	(lambda (s1 s2 k)
		(if (null? s1)
			(apply-continuation k s2)
			(memq-cps (car s1) s2 
				      (lambda (car-in-s2)
				      	      (union-cps (cdr s1) s2
				      	      	(lambda (union-with-cdr)
				      	      		(apply-continuation k 
				      	      			                (if car-in-s2
				      	      			                	union-with-cdr
				      	      			                	(cons (car s1) union-with-cdr))))))))))

(union-cps '(1 2 3) '(2 3 4) list)

(define free-vars-cps
	(lambda (exp k)
		(cond [(symbol? exp) (apply-continuation k (list exp))]
			  [(eq? (1st exp) 'lambda) 
			   (free-vars-cps (3rd exp)
			   				  (lambda (free-vars-from-body)
			   				  	      (remove-cps (car (2nd exp)))
			   				  	                   free-vars-from-body
			   				  	                   k))]
			   [else (free-vars-cps (1st exp) 
			     	            (lambda (free-vars-from-rator)
			     	            	    (free-vars-cps (2nd exp)
			     	            	    	           (lambda (free-vars-from-rand)
			     	            	    	           	       (union-cps free-vars-from-rator free-vars-from-rand k)))))])))

(define remove-cps
	(lambda (element ls k)
		(if (null? ls)
			(apply-continuation k '())
			(remove-cps element (cdr ls) 
				(lambda (removed-cdr)
					(apply-continuation k (if (eq? element (car ls))
											  removed-cdr
											  (cons (car ls) removed-cdr))))))))