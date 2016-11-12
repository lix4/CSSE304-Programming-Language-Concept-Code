;;Xiwen Li
;;Assignment 15

;Because the method in the assignment still calculate the whole fib each time, even though it saves the result in the hashtable.

(define apply-continuation
	(lambda (k v)
		(k v)))

;;1
(define member?-cps
	(lambda (sym ls k)
		(cond [(null? ls) (apply-continuation k #f)]
			  [(eq? (car ls) sym) (apply-continuation k #t)]
			  [else (member?-cps sym (cdr ls) k)])))

;;2
(define set?-cps
 	(lambda (ls k)
        (cond [(null? ls) (apply-continuation k #t)]
              [(not (pair? ls)) (apply-continuation k #f)]
              [else (set?-cps (cdr ls) 
              	              (lambda (set?-cdr-ls)
              	              		  (member?-cps (car ls) 
              	              		  	           (cdr ls)
              	              		  	           (lambda (car-ls-in-cdr-ls)
              	              		  	                   (apply-continuation k
              	              		  	                   	                   (if car-ls-in-cdr-ls
              	              		  	                   	                   	   #f
              	              		  	                   	                   	   set?-cdr-ls))))))])))

(define 1st-cps
	(lambda (ls k)
		(apply-continuation k (car ls))))

(define set-of-cps
 	(lambda (s k)
 		(if (null? s) 
 			(apply-continuation k '())
 			(set-of-cps (cdr s)
			 			(lambda (set-of-cdr-s)
			 				    (member?-cps (car s) (cdr s)
			 				    	         (lambda (car-ls-in-cdr-ls)
			 				    	         	     (apply-continuation k
			 				    	         	     	                 (if car-ls-in-cdr-ls
			 				    	         	     	                 	 set-of-cdr-s
			 				    	         	     	                 	 (cons (car s) set-of-cdr-s))))))))))

(define map-cps
	(lambda (proc ls k)
		(if (null? ls)
			(apply-continuation k '())
			(map-cps proc
				     (cdr ls)
				     (lambda (map-cps-cdr-ls)
				     	     (apply-continuation k 
				     	     	                 (cons (proc (car ls) (lambda (x) x)) map-cps-cdr-ls)))))))
 				    	         	     	                 	 
(define domain-cps
 		(lambda (rel k)
 				(set-of-cps (map-cps (lambda (x k) (1st-cps x (lambda (x) x))) 
 					                 rel 
 					                 (lambda (map-cps-results) map-cps-results))
 					        (lambda (set-of-cps-result)
 					                (apply-continuation k set-of-cps-result)))))  


(define make-cps
		(lambda (proc)
			(lambda (a k)
				(apply-continuation k (proc a)))))          	                 


(define andmap-cps
	(lambda (pred?-cps ls k)
		(cond
			[(null? ls) (apply-continuation k #t)]
			[else (pred?-cps (car ls) 
				　　　　　(lambda (pred-car-result) 
					　　　　　(if pred-car-result 
						　　　　　(andmap-cps pred?-cps (cdr ls) k)
						　　　　　(apply-continuation k #f))))])))

(define cps-snlist-recur
 		(lambda (seed item-proc list-proc)
 				(letrec ([helper (lambda (ls k)
	 									 (if (null? ls)
	 										 (apply-continuation k seed)
											 (let ([c (car ls)])
 												  (helper (cdr ls)
 												  	      (lambda (cdr-ls-result)
 												  	      	      	  (if (or (pair? c) (null? c))
 												  	      	      	  	  (helper (car ls)
 												  	      	      	  	  	      (lambda (car-ls-result)
 												  	      	      	  	  	      	 (apply-continuation k
 												  	      	      	  	  	      	    (list-proc car-ls-result cdr-ls-result (lambda (x) x)))))
 												  	      	      	  	  (apply-continuation k (item-proc c cdr-ls-result (lambda (x) x)))))))))])      	      	  	  
 				          helper)))

(define reverse-cps
	(lambda (a b k)
		(apply-continuation k (append b (list a)))))

(define sn-list-reverse-cps
		(cps-snlist-recur '() reverse-cps reverse-cps))

(define sn-list-occur-cps
	(lambda (s snlst k)
		((cps-snlist-recur 0
			         (lambda (x y k)
			         	(apply-continuation k
				         	(if (equal? x s)
				         		(+ 1 y)
				         		y)))
			         (lambda (x y k)
			         	(apply-continuation k (+ x y)))) snlst k)))

(define sn-list-depth-cps
	(lambda (snlist k)
		((cps-snlist-recur 1
			         (lambda (x y k) y)
			         (lambda (x y k) (max (+ x 1) y))) snlist k)))

(define memoize
	(lambda (f hash equiv?)
		(letrec ([ht (make-hashtable hash equiv?)]
				[func (lambda x
					(if (hashtable-contains? ht x)
						(hashtable-ref ht x 0)
					    (let ((a (apply f x)))
					    	(hashtable-set! ht x a)
					    	a)))])
				func)))


(define subst-leftmost
	(lambda (new old slist equality-pred?)
			(car (sublist-leftmost-rec new old slist equality-pred? #f '()))))

(define sublist-leftmost-rec
	(lambda (new old slist equality-pred? already-add? new-list)
		(cond
			[(null? slist) (list new-list already-add?)]
			[(equal? already-add? #t) (sublist-leftmost-rec new old (cdr slist) equality-pred? already-add? (call-with-values (lambda () (values new-list (list (car slist)))) append))]
			[(null? (car slist)) (sublist-leftmost-rec new old (cdr slist) equality-pred? already-add? (call-with-values (lambda () (values new-list '(()) )) append))]
			[(symbol? (car slist)) 
			(cond 
				[(and (equality-pred? old (car slist)) (not already-add?))
				(sublist-leftmost-rec new old (cdr slist) equality-pred? (not already-add?) (call-with-values (lambda () (values new-list (list new))) append))]
				[else (sublist-leftmost-rec new old (cdr slist) equality-pred? already-add? (call-with-values (lambda () (values new-list (list (car slist)))) append))]
				)]
			[else (let ((last (sublist-leftmost-rec new old (car slist) equality-pred? already-add? '())))
					(cond
						[(cadr last) (sublist-leftmost-rec new old (cdr slist) equality-pred? #t (call-with-values (lambda () (values new-list (list (car last)))) append))]
						[else (sublist-leftmost-rec new old (cdr slist) equality-pred? #f new-list)]
						)
				)]
			)
		)
	)


